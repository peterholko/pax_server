%% Author: Peter
%% Created: Feb 4, 2009
%% Description: TODO: Add description to army
-module(army).
-behaviour(gen_server).

%%
%% Include files
%%

-include("game.hrl").
-include("common.hrl").
-include("schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

%%
%% Exported Functions
%%
-export([move/3, attack/2, claim/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/2, stop/1]).

-record(module_data, {army,  
                      player_id, 
                      self,
                      visible = [],
                      observed_by = []}).

%%
%% API Functions
%%

move(ArmyId, DestX, DestY) ->
    gen_server:cast(global:whereis_name({army, ArmyId}), {'SET_STATE_MOVE', DestX, DestY}).

attack(ArmyId, TargetId) ->
    gen_server:cast(global:whereis_name({army, ArmyId}), {'SET_STATE_ATTACK', TargetId}).

claim(ArmyId, ClaimId) ->
    gen_server:cast(global:whereis_name({army, ArmyId}), {'SET_STATE_CLAIM', ClaimId}).

start(ArmyId, PlayerId) ->
    case db:read(army, ArmyId) of
        [Army] ->
            gen_server:start({global, {army, ArmyId}}, army, [Army, PlayerId], []);
        Any ->
            {error, Any}
    end.

init([Army, PlayerId]) 
  when is_tuple(Army),
       is_integer(PlayerId) ->
    process_flag(trap_exit, true),
    
    log4erl:debug("{~w} army_id: ~w player_id: ~w", [?MODULE, Army#army.id, PlayerId]),  
    
    {ok, #module_data{army = Army, player_id = PlayerId, self = self()}}.

terminate(_Reason, _) ->
    ok.

stop(ProcessId) 
  when is_pid(ProcessId) ->
    gen_server:cast(ProcessId, stop).

handle_cast({'SET_STATE_MOVE', DestX, DestY}, Data) ->
    log4erl:info("{~w} SET_STATE_MOVE", [?MODULE]),
    Army = Data#module_data.army,
    {NextX, NextY} = next_pos(Army#army.x, Army#army.y, DestX, DestY),
    ArmySpeed = get_army_speed(Army#army.id, NextX, NextY),
    
    case Army#army.state of
        ?STATE_NONE ->
            add_event_move(Data#module_data.self, ArmySpeed);
        ?STATE_ATTACK ->
            add_event_move(Data#module_data.self, ArmySpeed);
        ?STATE_MOVE ->
            add_event_move(Data#module_data.self, ArmySpeed);
        ?STATE_CLAIM ->
            claim:cancel(Army#army.id),        
            add_event_move(Data#module_data.self, ArmySpeed);
        _ ->
            add_event_move(Data#module_data.self, ArmySpeed)
    end,         
    
    NewArmy = state_move(Army, DestX, DestY),  
    NewData = Data#module_data {army = NewArmy},
    save_army(NewArmy),
    
    {noreply, NewData};

handle_cast({'SET_STATE_ATTACK', TargetId}, Data) ->
    log4erl:info("{~w} SET_STATE_ATTACK", [?MODULE]),
    Army = Data#module_data.army,
    TargetState = gen_server:call(global:whereis_name({army, TargetId}), {'GET_STATE', Army#army.id}),
    {NextX, NextY} = next_pos(Army#army.x, Army#army.y, TargetState#state.x, TargetState#state.y),
    ArmySpeed = get_army_speed(Army#army.id, NextX, NextY),
    
    if
        (Army#army.state =/= ?STATE_ATTACK) and (Army#army.state =/= ?STATE_COMBAT)->
            gen_server:cast(global:whereis_name(game_pid), {'CLEAR_EVENTS', Data#module_data.self}),
            gen_server:cast(global:whereis_name(game_pid), {'ADD_EVENT', Data#module_data.self, ?EVENT_ATTACK, none, speed_to_ticks(ArmySpeed)});
        true ->
            ok
    end,         
    
    
    NewArmy = state_attack(Data#module_data.army, TargetId),
    NewData = Data#module_data {army = NewArmy},
    save_army(NewArmy),
    
    {noreply, NewData};

handle_cast({'SET_STATE_RETREAT_MOVE'}, Data) ->
    log4erl:info("{~w} SET_STATE_RETREAT_MOVE", [?MODULE]),
    Army = Data#module_data.army,
    {LastX, LastY} = Army#army.last_pos,
    
    ArmySpeed = get_army_speed(Army#army.id, LastX, LastY),
    
    if
        Army#army.state =:= ?STATE_RETREAT ->
            gen_server:cast(global:whereis_name(game_pid), {'CLEAR_EVENTS', Data#module_data.self}),
            gen_server:cast(global:whereis_name(game_pid), {'ADD_EVENT', Data#module_data.self, ?EVENT_RETREAT_MOVE, none, speed_to_ticks(ArmySpeed)});
        true ->
            ok
    end,
    
    NewArmy = state_retreat_move(Data#module_data.army, LastX, LastY),
    NewData = Data#module_data {army = NewArmy},
    save_army(NewArmy),

    {noreply, NewData};   
 
handle_cast({'SET_STATE_LEAVE_MOVE'}, Data) ->
    log4erl:info("{~w} SET_STATE_LEAVE_MOVE", [?MODULE]),
    Army = Data#module_data.army,
    {LastX, LastY} = Army#army.last_pos,
    
    ArmySpeed = get_army_speed(Army#army.id, LastX, LastY),
    
    if
        Army#army.state =:= ?STATE_LEAVE ->
            gen_server:cast(global:whereis_name(game_pid), {'CLEAR_EVENTS', Data#module_data.self}),
            gen_server:cast(global:whereis_name(game_pid), {'ADD_EVENT', Data#module_data.self, ?EVENT_LEAVE_MOVE, none, speed_to_ticks(ArmySpeed)});
        true ->
            ok
    end,
    
    NewArmy = state_leave_move(Data#module_data.army, LastX, LastY),
    NewData = Data#module_data {army = NewArmy},
    save_army(NewArmy),

    {noreply, NewData};    

handle_cast({'SET_STATE_COMBAT', BattleId}, Data) ->
    Army = Data#module_data.army,
    
    gen_server:cast(global:whereis_name(game_pid), {'CLEAR_EVENTS', Data#module_data.self}),	
    
    NewArmy = state_combat(Army, BattleId),
    NewData = Data#module_data {army = NewArmy},
    save_army(NewArmy),
    
    {noreply, NewData};

handle_cast({'SET_STATE_RETREAT', BattleId}, Data) ->
    Army = Data#module_data.army,
    
    gen_server:cast(global:whereis_name(game_pid), {'CLEAR_EVENTS', Data#module_data.self}),
    NewArmy = state_retreat(Army, BattleId),
    NewData = Data#module_data {army = NewArmy},
    save_army(NewArmy),

    {noreply, NewData};

handle_cast({'SET_STATE_LEAVE', BattleId}, Data) ->
    Army = Data#module_data.army,
    
    gen_server:cast(global:whereis_name(game_pid), {'CLEAR_EVENTS', Data#module_data.self}),
    NewArmy = state_leave(Army, BattleId),
    NewData = Data#module_data {army = NewArmy},
    save_army(NewArmy),

    {noreply, NewData};

handle_cast({'SET_STATE_CLAIM', ClaimId}, Data) ->
    ?INFO("Set State Claim Id", ClaimId),
    Army = Data#module_data.army,
    
    add_event_claim(Data#module_data.self, ClaimId),    

    NewArmy = state_claim(Army),
    NewData = Data#module_data {army = NewArmy},
    save_army(NewArmy),

    {noreply, NewData};

handle_cast({'SET_STATE_NONE'}, Data) ->
    NewArmy = state_none(Data#module_data.army),
    NewData = Data#module_data {army = NewArmy},    
    save_army(NewArmy),
    
    {noreply, NewData};	

handle_cast({'ADD_WAYPOINT', X, Y}, Data) ->
    NewArmy = add_waypoint(Data#module_data.army, X, Y),
    NewData = Data#module_data {army = NewArmy},
    save_army(NewArmy),  
    
    {noreply, NewData};

handle_cast({'PROCESS_EVENT', _EventTick, EventData, EventType}, Data) ->
    ?INFO("Processing Event Type", EventType),
    Army = Data#module_data.army,

    case EventType of
        ?EVENT_MOVE ->
            NewArmy = do_move(Army, Data#module_data.self, Data#module_data.visible, Data#module_data.observed_by);
        ?EVENT_ATTACK ->
            NewArmy = do_attack(Army, Data#module_data.self, Data#module_data.visible, Data#module_data.observed_by);
        ?EVENT_RETREAT_MOVE ->
            battle:remove_army(Army#army.battle, Army#army.id),
            NewArmy = do_move(Army, Data#module_data.self, Data#module_data.visible, Data#module_data.observed_by);
        ?EVENT_LEAVE_MOVE ->
            battle:remove_army(Army#army.battle, Army#army.id),
            NewArmy = do_move(Army, Data#module_data.self, Data#module_data.visible, Data#module_data.observed_by);
        ?EVENT_CLAIM ->
            ClaimId = EventData,
            claim:complete(ClaimId),
            NewArmy = state_none(Army);
        ?EVENT_NONE ->
            NewArmy = Army
    end,     
 
    NewData = Data#module_data {army = NewArmy},

    save_army(NewData#module_data.army),
 
    {noreply, NewData};

handle_cast({'ADD_VISIBLE', _ArmyId, EntityId, EntityPid}, Data) ->
    VisibleList = Data#module_data.visible,
    NewVisibleList = [{EntityId, EntityPid} | VisibleList],		
    NewData = Data#module_data { visible = NewVisibleList },
    
    update_perception(Data#module_data.player_id),
    
    {noreply, NewData};

handle_cast({'REMOVE_VISIBLE', _ArmyId, EntityId, EntityPid}, Data) ->
    VisibleList = Data#module_data.visible,
    NewVisibleList = lists:delete({EntityId, EntityPid}, VisibleList),
    NewData = Data#module_data { visible = NewVisibleList },
    
    update_perception(Data#module_data.player_id),
    
    {noreply, NewData};

handle_cast({'ADD_OBSERVED_BY', _ArmyId, EntityId, EntityPid}, Data) ->
    ObservedByList = Data#module_data.observed_by,
    NewObservedByList = [{EntityId, EntityPid} | ObservedByList],
    NewData = Data#module_data { observed_by = NewObservedByList },
    
    {noreply, NewData};

handle_cast({'REMOVE_OBSERVED_BY', _ArmyId, EntityId, EntityPid}, Data) ->
    ObservedByList = Data#module_data.observed_by,
    NewObservedByList = lists:delete({EntityId, EntityPid}, ObservedByList),
    NewData = Data#module_data { observed_by = NewObservedByList },
    
    {noreply, NewData};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({'DAMAGE_UNIT', UnitId, Damage}, _From, Data) ->
    
    Army = Data#module_data.army,
    Units = Army#army.units,
   
    case gb_sets:is_member(UnitId, Units) of
        true ->
            [Unit] = db:dirty_read(unit, UnitId),            
            [UnitType] = db:dirty_read(unit_type, Unit#unit.type),
            TotalHp = Unit#unit.size * UnitType#unit_type.total_hp,            
            io:fwrite("Army ~w - Damage: ~w~n", [Army#army.id, Damage]),
            
            if
                Damage >= TotalHp ->
                    io:fwrite("Army ~w - Unit Destroyed.~n", [Army#army.id]),
                    db:dirty_delete(unit, UnitId),
                    NewUnits = gb_sets:delete(UnitId, Units);
                true ->
                    Killed = Damage div UnitType#unit_type.total_hp,
                    io:fwrite("Army ~w - Units killed: ~w~n", [Army#army.id, Killed]),
                    
                    NewSize = Unit#unit.size - Killed,
                    NewUnit = Unit#unit {size = NewSize},
                    db:dirty_write(NewUnit),                    
                    io:fwrite("Army ~w - Units size: ~w~n", [Army#army.id, NewSize]),
                    
                    NewUnits = gb_sets:add(UnitId, Units)
            end,
            
            NewArmy = Army#army { units = NewUnits },                    
            NewData = Data#module_data {army = NewArmy};
        false ->
            NewData = Data
    end,   
    
    NewerData = get_army_status(NewData),
    NewerArmy = NewerData#module_data.army,
    save_army(NewerArmy),
    
    {reply, NewerArmy#army.state, NewerData};

handle_call({'TRANSFER_UNIT', _SourceId, UnitId, TargetId, TargetAtom}, _From, Data) ->
    io:fwrite("army - transfer unit.~n"),
    Army = Data#module_data.army,    
    Units = Army#army.units,

    case gb_sets:is_member(UnitId, Units) of
        true ->
            [Unit] = db:dirty_read(unit, UnitId),                      
            TargetPid = object:get_pid(TargetAtom, TargetId),

            case gen_server:call(TargetPid, {'ON_SAME_TILE', Army#army.x, Army#army.y}) of
                true ->
                    case gen_server:call(TargetPid, {'RECEIVE_UNIT', TargetId, Unit, Data#module_data.player_id}) of
                        {receive_unit, success} ->
                            db:dirty_delete(unit, Unit),
                            NewUnits = gb_sets:delete(UnitId, Units),
                            NewArmy = Army#army {units = NewUnits},
                            NewData = Data#module_data{army = NewArmy},
                            TransferUnitInfo = {transfer_unit, success};
                        Error ->
                            TransferUnitInfo = Error,
                            NewData = Data
                    end;
                false ->
                    TransferUnitInfo = {transfer_unit, not_same_tile},
                    NewData = Data
            end;
        false ->
            TransferUnitInfo = {transfer_unit, unit_is_not_member},
            NewData = Data
    end,

    save_army(NewData#module_data.army),
    
    {reply, TransferUnitInfo , NewData};

handle_call({'RECEIVE_UNIT', _TargetId, Unit, PlayerId}, _From, Data) ->
    io:fwrite("army - receive unit.~n"),
    Army = Data#module_data.army,
    Units = Army#army.units,
    
    if
        PlayerId =:= Data#module_data.player_id ->
            %Check to see if unit already exists
            UnitResult = gb_sets:is_member(Unit#unit.id, Units),            
            
            if
                UnitResult =:= false ->                    
                    NewUnit = Unit#unit {entity_id = Army#army.id},
                    db:dirty_write(NewUnit),       

                    NewUnits = gb_sets:add(Unit#unit.id, Units),
                    NewArmy = Army#army {units = NewUnits},
                    NewData = Data#module_data {army = NewArmy},
                    ReceiveUnitInfo = {receive_unit, success};                
                true ->
                    NewData = Data,
                    ReceiveUnitInfo = {receive_unit, error}
            end;               
        true ->
            NewData = Data,
            ReceiveUnitInfo = {receive_unit, error}
    end,

    save_army(NewData#module_data.army),
    
    {reply, ReceiveUnitInfo, NewData};    

handle_call({'TRANSFER_ITEM', ItemId, TargetId, TargetAtom}, _From, Data) ->
    log4erl:info("Army - TRANSFER_ITEM"),
    Army = Data#module_data.army,

    TargetPid = object:get_pid(TargetAtom, TargetId),
    case entity:on_same_tile(TargetPid, Army#army.x, Army#army.y) of
        true ->
            TargetPlayerId = entity:get_player_id(TargetPid), 
            item:transfer(ItemId, TargetId, TargetPlayerId),
            TransferItemInfo = {transfer_item, success};
        false ->
            TransferItemInfo = {transfer_item, not_same_tile}
    end,

    {reply, TransferItemInfo, Data};

handle_call({'GET_INFO', PlayerId}, _From, Data) ->    
    Army = Data#module_data.army,   

    case Army#army.player_id =:= PlayerId of
        true ->
            Units = db:dirty_index_read(unit, Army#army.id, #unit.entity_id),
            UnitsInfoTuple = unit:tuple_form(Units),

            %Convert items record to tuple packet form
            NewItems = db:dirty_index_read(item, {Army#army.id, PlayerId}, #item.ref),
            ItemsTuple = item:tuple_form(NewItems),

            ArmyInfo = {detailed, 
                        Army#army.id, 
                        Army#army.player_id, 
                        Army#army.name,
                        kingdom:get_name(Army#army.player_id),
                        UnitsInfoTuple,
                        ItemsTuple};
        false ->
            ArmyInfo = {generic, 
                        Army#army.id,
                        Army#army.player_id, 
                        Army#army.name, 
                        kingdom:get_name(Army#army.player_id)}
    end,            
    
    io:fwrite("army - ArmyInfo: ~w~n", [ArmyInfo]),
    {reply, ArmyInfo , Data};

%Used by battle module for detailed army information
handle_call({'GET_INFO'}, _From, Data) ->
    Army = Data#module_data.army,

    Units = db:dirty_index_read(unit, Army#army.id, #unit.entity_id),
    UnitsInfoTuple = unit:tuple_form(Units),
    ArmyInfo = {Army#army.id, 
                Army#army.player_id, 
                Army#army.name,
                kingdom:get_name(Army#army.player_id),
                UnitsInfoTuple},

    {reply, ArmyInfo, Data};

handle_call({'GET_UNITS'}, _From, Data) ->
    Army = Data#module_data.army,
    Units = db:dirty_index_read(unit, Army#army.id, #unit.entity_id),
    {reply, Units, Data};

handle_call({'GET_UNIT', UnitId}, _From, Data) ->
    Unit = unit:get_unit(UnitId),
    {reply, Unit, Data};

handle_call({'GET_STATE', _ArmyId}, _From, Data) ->
    Army = Data#module_data.army,
    
    State = #state { id = Army#army.id, 
                     player_id = Army#army.player_id, 
                     type = ?OBJECT_ARMY,
                     subtype = ?OBJECT_BASIC,
                     state = Army#army.state,
                     x = Army#army.x,
                     y = Army#army.y},
    
    {reply, State, Data};

handle_call({'GET_ID'}, _From, Data) ->
    Army = Data#module_data.army,
    {reply, Army#army.id, Data};

handle_call({'GET_PLAYER_ID'}, _From, Data) ->
    {reply, Data#module_data.player_id, Data};

handle_call({'GET_TYPE'}, _From, Data) ->
    {reply, ?OBJECT_ARMY, Data};

handle_call('GET_VISIBLE', _From, Data) ->
    {reply, Data#module_data.visible, Data};

handle_call('GET_OBSERVED_BY', _From, Data) ->
    {reply, Data#module_data.observed_by, Data};

handle_call('GET_SUBSCRIPTION_DATA', _From, Data) ->
    Army = Data#module_data.army,	
    {reply, {Army#army.x, Army#army.y, Data#module_data.visible, Data#module_data.observed_by}, Data};

handle_call({'ON_SAME_TILE', X, Y}, _From, Data) ->
    Army = Data#module_data.army,   
    {reply, (Army#army.x =:= X) and (Army#army.y =:= Y), Data};

handle_call(Event, From, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
                              {line, ?LINE},
                              {self, self()}, 
                              {message, Event}, 
                              {from, From}
                             ]),
    {noreply, Data}.

handle_info(Info, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
                              {line, ?LINE},
                              {self, self()}, 
                              {message, Info}]),
    {noreply, Data}.

code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.

%%
%% Local Functions
%%

do_move(Army, ArmyPid, VisibleList, ObservedByList) ->    
    %% Move army coordinates
    io:fwrite("Army#army.dest: ~w~n", [Army#army.dest]),
    [{DestX, DestY} | DestRest] = Army#army.dest, 
    {NewArmyX, NewArmyY} = next_pos(Army#army.x, Army#army.y, DestX, DestY),
    
    %% Set any newly discovered tiles
    set_discovered_tiles(Army#army.player_id, Army#army.id, NewArmyX, NewArmyY),
 
    %% Update subscription model
    EveryObjectList = gen_server:call(global:whereis_name(game_pid), 'GET_OBJECTS'),
    {ok, SubscriptionPid} = subscription:start(Army#army.id),
    subscription:update_perception(SubscriptionPid, Army#army.id, ArmyPid, NewArmyX, NewArmyY, EveryObjectList, VisibleList, ObservedByList),
    
    %% Toggle player's perception has been updated.
    update_perception(Army#army.player_id),  
 
    %% Toggle observedByList perception has been updated due to army move.
    entity_update_perception(ObservedByList),    	
        
    %% Check if destination has been reached and if destination list is empty
    case check_destination(NewArmyX =:= DestX, NewArmyY =:= DestY, length(DestRest)) of
        final ->
            io:fwrite("Final destination reached.~n"),
            NewArmy = state_none(Army, NewArmyX, NewArmyY);
        waypoint ->            
            io:fwrite("Next Waypoint.~n"),
            {NextX, NextY} = next_pos(NewArmyX, NewArmyY, DestX, DestY),
            ArmySpeed = get_army_speed(Army#army.id, NextX, NextY),
            game:add_event(ArmyPid, ?EVENT_MOVE, none, speed_to_ticks(ArmySpeed)),   
            NewArmy = event_move_next_dest(Army, NewArmyX, NewArmyY, DestRest);
        moving ->
            io:fwrite("Moving.~n"),
            {NextX, NextY} = next_pos(NewArmyX, NewArmyY, DestX, DestY),
            ArmySpeed = get_army_speed(Army#army.id, NextX, NextY),
            game:add_event(ArmyPid, ?EVENT_MOVE, none, speed_to_ticks(ArmySpeed)),   
            NewArmy = event_move(Army, NewArmyX, NewArmyY)
    end,
    
    %Update the last position
    update_last_pos(NewArmy).

do_attack(Army, ArmyPid, VisibleList, ObservedByList) ->    
    TargetState = gen_server:call(global:whereis_name({army, Army#army.target}), {'GET_STATE', Army#army.id}),
    {NewArmyX, NewArmyY} = next_pos(Army#army.x, Army#army.y, TargetState#state.x, TargetState#state.y),
    
    if	
        (NewArmyX =:= TargetState#state.x) and (NewArmyY =:= TargetState#state.y) -> 
            io:fwrite("Army - create battle~n"),            
            BattleId = counter:increment(battle) + 1000000,
            battle:create(BattleId, TargetState#state.x, TargetState#state.y),
            battle:setup(BattleId, Army#army.id, Army#army.target),
            gen_server:cast(global:whereis_name({army, Army#army.target}), {'SET_STATE_COMBAT', BattleId}),
            
            NewArmy = state_combat(Army, BattleId, NewArmyX, NewArmyY);
        true ->
            {NextX, NextY} = next_pos(NewArmyX, NewArmyY, TargetState#state.x, TargetState#state.y),
            ArmySpeed = get_army_speed(Army#army.id, NextX, NextY),
            game:add_event(ArmyPid, ?EVENT_ATTACK, none, speed_to_ticks(ArmySpeed)),            
            NewArmy = event_move(Army, NewArmyX, NewArmyY)
    end,

    %% Set any newly discovered tiles
    set_discovered_tiles(Army#army.player_id, Army#army.id, NewArmyX, NewArmyY),

    %% Update subscription model
    EveryObjectList = gen_server:call(global:whereis_name(game_pid), 'GET_OBJECTS'),
    {ok, SubscriptionPid} = subscription:start(Army#army.id),
    subscription:update_perception(SubscriptionPid, Army#army.id, ArmyPid, NewArmyX, NewArmyY, EveryObjectList, VisibleList, ObservedByList),
    
    %Toggle player's perception has been updated.
    update_perception(Army#army.player_id),  
    
    %Toggle observedByList perception has been updated due to army move.
    entity_update_perception(ObservedByList),    
	
    update_last_pos(NewArmy).

next_pos(ArmyX, ArmyY, DestX, DestY) ->
    DiffX = DestX - ArmyX,
    DiffY = DestY - ArmyY,
    
    if
        DiffX > 0 ->
            NewArmyX = ArmyX + 1;
        DiffX < 0 ->
            NewArmyX = ArmyX - 1;
        true ->
            NewArmyX = ArmyX
    end,
    
    if
        DiffY > 0 ->
            NewArmyY = ArmyY + 1;
        DiffY < 0 ->
            NewArmyY = ArmyY - 1;
        true ->
            NewArmyY = ArmyY
    end,
    
    {NewArmyX, NewArmyY}.

event_move(Army, NewX, NewY) ->
    Army#army{x = NewX,
              y = NewY}.  

event_move_next_dest(Army, NewX, NewY, NextDest) ->
    Army#army{x = NewX,
              y = NewY,
              dest = NextDest}.

update_last_pos(Army) ->
    LastPos = {Army#army.x, Army#army.y},
    Army#army { last_pos = LastPos}.

add_event_move(Pid, ArmySpeed) ->
    game:clear_events(Pid),
    game:add_event(Pid, ?EVENT_MOVE, none, speed_to_ticks(ArmySpeed)).

add_event_claim(Pid, ClaimId) ->
    game:clear_events(Pid),
    game:add_event(Pid, ?EVENT_CLAIM, ClaimId, ?CLAIM_TICK).
 
state_move(Army, DestX, DestY) ->
    Army#army{dest = [{DestX, DestY}],
              state = ?STATE_MOVE}.

state_attack(Army, TargetId) ->
    Army#army{state = ?STATE_ATTACK,
              target = TargetId}.

state_retreat(Army, BattleId) ->
    Army#army{state = ?STATE_RETREAT,
              battle = BattleId}.

state_retreat_move(Army, LastX, LastY) ->
    Army#army{dest = [{LastX, LastY}],
              state = ?STATE_RETREAT_MOVE}.

state_leave(Army, BattleId) ->
    Army#army{state = ?STATE_LEAVE,
              battle = BattleId}.

state_leave_move(Army, LastX, LastY) ->
    Army#army{dest = [{LastX, LastY}],
              state = ?STATE_LEAVE_MOVE}.

state_combat(Army, BattleId) ->
    Army#army{state = ?STATE_COMBAT,
              battle = BattleId}.

state_combat(Army, BattleId, X, Y) ->
    Army#army{state = ?STATE_COMBAT,
              battle = BattleId,
              x = X,
              y = Y}.

state_dead(Army) ->
    Army#army{state = ?STATE_DEAD}.

state_claim(Army) ->
    Army#army{state = ?STATE_CLAIM}.

state_none(Army, X, Y) ->
    Army#army{state = ?STATE_NONE,
              x = X,
              y = Y}.

state_none(Army) ->
    Army#army{state = ?STATE_NONE}. 

get_army_status(Data) ->   
    Army = Data#module_data.army,
    NumUnits = gb_sets:size(Army#army.units),
    
    if
        NumUnits =:= 0 ->
            NewArmy = state_dead(Army);
        true ->
            NewArmy = Army
    end,

    Data#module_data {army = NewArmy}.

get_army_speed(ArmyId, X, Y) ->
    ArmySpeed = unit:highest_unit_movement(ArmyId),
    TileType = map:get_tile_type(X,Y),
    Speed = ArmySpeed * tile_modifier(TileType),
    io:fwrite("Speed: ~w~n",[Speed]),
    Speed.

speed_to_ticks(Speed) ->
    (1000 div ?GAME_LOOP_TICK) * Speed.

tile_modifier(?TILE_MOUNTAIN) ->
    ?TILE_MOUNTAIN_SPEED;
tile_modifier(?TILE_FOREST) ->
    ?TILE_FOREST_SPEED;
tile_modifier(?TILE_PLAINS) ->
    ?TILE_PLAINS_SPEED;
tile_modifier(?TILE_SWAMP) ->
    ?TILE_SWAMP_SPEED;
tile_modifier(_) ->
    1.    

entity_update_perception(EntityList) ->
    F = fun({_EntityId, EntityPid}) ->
            case gen_server:call(EntityPid, {'GET_TYPE'}) of
                ?OBJECT_ARMY ->                                      
                    check_player_online(EntityPid);
                ?OBJECT_CITY ->
                    check_player_online(EntityPid);
                _OtherTypes ->
                    no_update
            end
        end,
    lists:foreach(F, EntityList).
            
check_player_online(EntityPid) ->
    PlayerId = gen_server:call(EntityPid, {'GET_PLAYER_ID'}),
    case gen_server:call(global:whereis_name(game_pid), {'IS_PLAYER_ONLINE', PlayerId}) of
        true ->
            update_perception(PlayerId);
        false ->
            no_update
    end.
  
update_perception(PlayerId) ->   

    case player:get_type(PlayerId) of
        ?PLAYER_HUMAN ->
            %Toggle within game state that player's perception has been updated.
            gen_server:cast(global:whereis_name(game_pid),{'UPDATE_PERCEPTION', PlayerId});
        ?PLAYER_COMPUTER ->
            no_update;
        _PlayerType ->
            no_update      
    end.

set_discovered_tiles(PlayerId, ArmyId, X, Y) ->
    case player:get_type(PlayerId) of
        ?PLAYER_HUMAN ->
            gen_server:cast(global:whereis_name({player, PlayerId}), {'SET_DISCOVERED_TILES', ArmyId, X, Y});
        ?PLAYER_COMPUTER ->
            ok;
        _ ->
            ok
    end.

check_destination(_X = true, _Y = true, _DestList = 0) ->
    final;
check_destination(_X = true, _Y = true, _DestList) ->
    waypoint;
check_destination(_X, _Y, _DestList) ->
    moving.

add_waypoint(Army, X, Y) ->
    NewDest = [{X, Y} | Army#army.dest],
    Army#army {dest = NewDest}.

save_army(Army) ->
    db:dirty_write(Army).
