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
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-export([start/2, stop/1]).

-record(module_data, {
          army,  
          units,       
          player_id, 
          self,
		  visible = [],
		  observed_by = [],
          save_army = false
         }).

%%
%% API Functions
%%

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
    
    io:fwrite("army - army_id: ~w player_id: ~w~n", [Army#army.id, PlayerId]),  
    
    ListUnits = db:index_read(unit, Army#army.id, #unit.entity_id),
    DictUnits = dict:new(),
    NewDictUnits = unit:init_units(ListUnits, DictUnits),
    
    {ok, #module_data{army = Army, units = NewDictUnits, player_id = PlayerId, self = self()}}.

terminate(_Reason, _) ->
    ok.

stop(ProcessId) 
  when is_pid(ProcessId) ->
    gen_server:cast(ProcessId, stop).

handle_cast({'SET_STATE_MOVE', DestX, DestY}, Data) ->
    io:fwrite("army - set_state_move ~n"),
    
    Army = Data#module_data.army,
    ArmySpeed = get_army_speed(Army#army.id),
    
    if
        (Army#army.state =/= ?STATE_MOVE) and (Army#army.state =/= ?STATE_COMBAT) ->
			io:fwrite("army - ArmyId: ~w ArmyState: ~w expression: ~w~n", [Army#army.id, Army#army.state, (Army#army.state =/= ?STATE_MOVE) and (Army#army.state =/= ?STATE_COMBAT)]),
            gen_server:cast(global:whereis_name(game_pid), {'CLEAR_EVENTS', Data#module_data.self}),
        	gen_server:cast(global:whereis_name(game_pid), {'ADD_EVENT', Data#module_data.self, ?EVENT_MOVE, none, speed_to_ticks(ArmySpeed)});
        true ->
            ok
    end,         
    
    NewArmy = state_move(Army, DestX, DestY),  
    NewData = Data#module_data {army = NewArmy, save_army = true},
    
    {noreply, NewData};

handle_cast({'SET_STATE_ATTACK', TargetId}, Data) ->
    io:fwrite("army - set_state_attack ~n"),
    
    Army = Data#module_data.army,
    ArmySpeed = get_army_speed(Army#army.id),
    
   	if
        (Army#army.state =/= ?STATE_ATTACK) and (Army#army.state =/= ?STATE_COMBAT)->
            gen_server:cast(global:whereis_name(game_pid), {'CLEAR_EVENTS', Data#module_data.self}),
        	gen_server:cast(global:whereis_name(game_pid), {'ADD_EVENT', Data#module_data.self, ?EVENT_ATTACK, none, speed_to_ticks(ArmySpeed)});
        true ->
            ok
    end,         
    
    
    NewArmy = state_attack(Data#module_data.army, TargetId),
    NewData = Data#module_data {army = NewArmy, save_army = true},
    
    {noreply, NewData};

handle_cast({'SET_STATE_COMBAT', BattleId}, Data) ->
	Army = Data#module_data.army,
	
    gen_server:cast(global:whereis_name(game_pid), {'CLEAR_EVENTS', Data#module_data.self}),	
	
    NewArmy = state_combat(Army, BattleId),
   	NewData = Data#module_data {army = NewArmy, save_army = true},
	
    {noreply, NewData};

handle_cast({'SET_STATE_NONE'}, Data) ->

	NewArmy = state_none(Data#module_data.army),
   	NewData = Data#module_data {army = NewArmy, save_army = true},    
    
	{noreply, NewData};	

handle_cast({'PROCESS_EVENT', _, EventType}, Data) ->
    
    case EventType of
		?EVENT_MOVE ->
            NewArmy = do_move(Data#module_data.army, Data#module_data.self, Data#module_data.visible, Data#module_data.observed_by),
            NewData = Data#module_data {army = NewArmy};
        ?EVENT_ATTACK ->
            NewArmy = do_attack(Data#module_data.army, Data#module_data.self, Data#module_data.visible, Data#module_data.observed_by),
            NewData = Data#module_data {army = NewArmy};
        ?EVENT_NONE ->
            NewData = Data
    end,      
    
    {noreply, NewData};

handle_cast({'ADD_VISIBLE', EntityId, EntityPid}, Data) ->
	VisibleList = Data#module_data.visible,
	NewVisibleList = [{EntityId, EntityPid} | VisibleList],
	NewData = Data#module_data { visible = NewVisibleList },
	
	{noreply, NewData};

handle_cast({'REMOVE_VISIBLE', EntityId, EntityPid}, Data) ->
	VisibleList = Data#module_data.visible,
	NewVisibleList = lists:delete({EntityId, EntityPid}, VisibleList),
	NewData = Data#module_data { visible = NewVisibleList },
	
	{noreply, NewData};

handle_cast({'ADD_OBSERVED_BY', EntityId, EntityPid}, Data) ->
	ObservedByList = Data#module_data.observed_by,
	NewObservedByList = [{EntityId, EntityPid} | ObservedByList],
	NewData = Data#module_data { observed_by = NewObservedByList },
	
	{noreply, NewData};

handle_cast({'REMOVE_OBSERVED_BY', EntityId, EntityPid}, Data) ->
	ObservedByList = Data#module_data.observed_by,
	NewObservedByList = lists:delete({EntityId, EntityPid}, ObservedByList),
	NewData = Data#module_data { observed_by = NewObservedByList },
	
	{noreply, NewData};

handle_cast('UPDATE_PERCEPTION', Data) ->
	
	Army = Data#module_data.army,	
	subscription(Army#army.id, self(), Army#army.x, Army#army.y, Data#module_data.visible, Data#module_data.observed_by),

	{noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({'DAMAGE_UNIT', UnitId, Damage}, _From, Data) ->

    Units = Data#module_data.units,
    Unit = unit:get_unit(UnitId, Units),
    
    if
        Unit =/= none ->
            
            [UnitType] = db:dirty_read(unit_type, Unit#unit.type),
    		TotalHp = Unit#unit.size * UnitType#unit_type.max_hp,
            Army = Data#module_data.army,
                                          
            io:fwrite("Army ~w - Damage: ~w~n", [Army#army.id, Damage]),
            
            if
                Damage >= TotalHp ->
					io:fwrite("Army ~w - Unit Destroyed.~n", [Army#army.id]),
                    NewUnits = dict:erase(UnitId, Units);
                true ->
                    Killed = Damage div UnitType#unit_type.max_hp,
                    
					io:fwrite("Army ~w - Units killed: ~w~n", [Army#army.id, Killed]),
					
                    NewSize = Unit#unit.size - Killed,
                    NewUnit = Unit#unit {size = NewSize},
					
					io:fwrite("Army ~w - Units size: ~w~n", [Army#army.id, NewSize]),
					
                    NewUnits = dict:store(UnitId, NewUnit, Units)
            end,
        						
        	NewData = Data#module_data {units = NewUnits};
        true ->
            NewData = Data
    end,   
    
	ArmyStatus = get_army_status(Data),
	
	{reply, ArmyStatus, NewData};

handle_call({'TRANSFER_UNIT', UnitId, TargetId, TargetAtom}, _From, Data) ->
    
    io:fwrite("army - transfer unit.~n"),
    
    Units = Data#module_data.units,
    UnitResult = dict:is_key(UnitId, Units),
    
    if
        UnitResult =/= false ->
            Unit = dict:fetch(UnitId, Units),
            
            case gen_server:call(global:whereis_name({TargetAtom, TargetId}), {'RECEIVE_UNIT', Unit, Data#module_data.player_id}) of
                {receive_unit, success} ->
                    NewUnits = dict:erase(UnitId, Units),
                    NewData = Data#module_data {units = NewUnits, save_army = true},
                    TransferUnitInfo = {transfer_unit, success};
                Error ->
                    TransferUnitInfo = Error,
                    NewData = Data
            end;
        true ->
            TransferUnitInfo = {transfer_unit, error},
            NewData = Data
    end,         		
    
	{reply, TransferUnitInfo , NewData};
                
handle_call({'RECEIVE_UNIT', Unit, PlayerId}, _From, Data) ->
    
    io:fwrite("army - receive unit.~n"),
    
    Army = Data#module_data.army,
    Units = Data#module_data.units,
    
    if
        PlayerId =:= Data#module_data.player_id ->
            %Check to see if unit already exists
            UnitResult = dict:is_key(Unit#unit.id, Units),
                        
            if
                UnitResult =:= false ->
                    
                    NewUnit = Unit#unit {entity_id = Army#army.id},
                    NewUnits = dict:store(Unit#unit.id, NewUnit, Units),
                    NewData = Data#module_data {units = NewUnits, save_army = true},
            		ReceiveUnitInfo = {receive_unit, success};
                
                true ->
    				NewData = Data,
                    ReceiveUnitInfo = {receive_unit, error}
            end;               
        true ->
			NewData = Data,
            ReceiveUnitInfo = {receive_unit, error}
    end,
    
    {reply, ReceiveUnitInfo, NewData};    

handle_call({'GET_INFO'}, _From, Data) ->
	
    Army = Data#module_data.army,    
    Units = Data#module_data.units,
                   
    %Convert record to tuple packet form
    UnitsInfoTuple = unit:units_tuple(Units),
	ArmyInfo = {Army#army.id, Army#army.player_id, UnitsInfoTuple},

    io:fwrite("army - ArmyInfo: ~w~n", [ArmyInfo]),
	{reply, ArmyInfo , Data};

handle_call({'GET_UNITS'}, _From, Data) ->
    {reply, Data#module_data.units, Data};

handle_call({'GET_UNIT', UnitId}, _From, Data) ->
    Units = Data#module_data.units,
    Unit = unit:get_unit(UnitId, Units),
    {reply, Unit, Data};

handle_call({'GET_STATE', _ArmyId}, _From, Data) ->
    Army = Data#module_data.army,
	
	State = #state { id = Army#army.id, 
					 player_id = Army#army.player_id, 
					 type = ?OBJECT_ARMY,
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

handle_call({'GET_VISIBLE_LIST'}, _From, Data) ->
    {reply, Data#module_data.visible, Data};

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
	{NewArmyX, NewArmyY} = move(Army#army.x, Army#army.y, Army#army.dest_x, Army#army.dest_y),
	
	%% Set any newly discovered tiles
    gen_server:cast(global:whereis_name({player, Army#army.player_id}), {'SET_DISCOVERED_TILES', Army#army.id, NewArmyX, NewArmyY}),
	
	%% Update subscription model
	subscription(Army#army.id, ArmyPid, NewArmyX, NewArmyX, VisibleList, ObservedByList),
    
	%% Update army's state
	if	
        (NewArmyX =:= Army#army.dest_x) and (NewArmyY =:= Army#army.dest_y) ->
            NewArmy = state_none(Army, NewArmyX, NewArmyY);
		true ->
            ArmySpeed = get_army_speed(Army#army.id),
			gen_server:cast(global:whereis_name(game_pid), {'ADD_EVENT', ArmyPid, ?EVENT_MOVE, none, speed_to_ticks(ArmySpeed)}),
	        NewArmy = event_move(Army, NewArmyX, NewArmyY)
	end,
    
	NewArmy.

do_attack(Army, ArmyPid, VisibleList, ObservedByList) ->    
    TargetState = gen_server:call(global:whereis_name({army, Army#army.target}), {'GET_STATE', Army#army.id}),
	{NewArmyX, NewArmyY} = move(Army#army.x, Army#army.y, TargetState#state.x, TargetState#state.y),
    
	if	
        (NewArmyX =:= TargetState#state.x) and (NewArmyY =:= TargetState#state.y) -> 
			
			BattleId = counter:increment(battle),
            battle:create(BattleId, TargetState#state.x, TargetState#state.y),
			battle:setup(BattleId, Army#army.id, Army#army.target),
			gen_server:cast(global:whereis_name({army, Army#army.target}), {'SET_STATE_COMBAT', BattleId}),
			
            NewArmy = state_combat(Army, BattleId, NewArmyX, NewArmyY);
		true ->
            ArmySpeed = get_army_speed(Army#army.id),
			gen_server:cast(global:whereis_name(game_pid), {'ADD_EVENT', ArmyPid, ?EVENT_ATTACK, none, speed_to_ticks(ArmySpeed)}),
			NewArmy = event_move(Army, NewArmyX, NewArmyY)
	end,
    
    NewArmy.

move(ArmyX, ArmyY, DestX, DestY) ->
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

subscription(ArmyId, ArmyPid, ArmyX, ArmyY, VisibleList, ObservedByList) ->
						
	NewVisibleList = remove_visible_list(ArmyId, ArmyPid, ArmyX, ArmyY, VisibleList),
	NewObservedByList = remove_observed_by_list(ArmyId, ArmyPid, ArmyX, ArmyY, ObservedByList),
	
	ObjectList = gen_server:call(global:whereis_name(game_pid), {'GET_OBJECTS'}),
	
	VisibleCandidateList = ObjectList -- NewVisibleList, 
	ObservedByCandidateList = ObjectList -- NewObservedByList,	

	add_visible_list(ArmyId, ArmyPid, ArmyX, ArmyY, VisibleCandidateList),
	add_observed_by_list(ArmyId, ArmyPid, ArmyX, ArmyY, ObservedByCandidateList).

remove_visible_list(ArmyId, ArmyPid, ArmyX, ArmyY, Visible) ->	
	F = fun({EntityId, EntityPid}, VisibleList) ->				
				Distance = calc_distance(ArmyX, ArmyY, EntityPid),
				
				if
					Distance >= 50 ->
						NewVisibleList = lists:delete({EntityId, EntityPid}, VisibleList),
						gen_server:cast(ArmyPid, {'REMOVE_VISIBLE', EntityId, EntityPid}),										
						gen_server:cast(EntityPid, {'REMOVE_OBSERVED_BY', ArmyId, ArmyPid});
					true ->
						NewVisibleList = VisibleList
				end,
				NewVisibleList
		end,				
	
    lists:foldl(F, Visible, Visible).

remove_observed_by_list(ArmyId, ArmyPid, ArmyX, ArmyY, ObservedBy) ->
	F = fun({EntityId, EntityPid}, ObservedByList) ->				
				Distance = calc_distance(ArmyX, ArmyY, EntityPid),
				
				if
					Distance >= 50 ->
						NewObservedByList = lists:delete({EntityId, EntityPid}, ObservedByList),
						gen_server:cast(ArmyPid, {'REMOVE_OBSERVED_BY', EntityId, EntityPid}),	
						gen_server:cast(EntityPid, {'REMOVE_VISIBLE', ArmyId, ArmyPid});										
					true ->
						NewObservedByList = ObservedByList
				end,
				NewObservedByList
		end,				
	
    lists:foldl(F, ObservedBy, ObservedBy).

add_visible_list(ArmyId, ArmyPid, ArmyX, ArmyY, VisibleCandidateList) ->
	F2 = fun({EntityId, EntityPid}) ->
				 Distance = calc_distance(ArmyX, ArmyY, EntityPid),				 
				 
				 if
					 Distance < 50 ->
						gen_server:cast(ArmyPid, {'ADD_VISIBLE', EntityId, EntityPid}),
						gen_server:cast(EntityPid, {'ADD_OBSERVED_BY', ArmyId, ArmyPid});	
					 true ->
						 ok
				 end
		 end,
	
	lists:foreach(F2, VisibleCandidateList).

add_observed_by_list(ArmyId, ArmyPid, ArmyX, ArmyY, ObservedByCandidateList) ->
	F2 = fun({EntityId, EntityPid}) ->
				 Distance = calc_distance(ArmyX, ArmyY, EntityPid),				 
				 
				 if
					 Distance < 50 ->
						gen_server:cast(ArmyPid, {'ADD_OBSERVED_BY', EntityId, EntityPid}),
						gen_server:cast(EntityPid, {'ADD_VISIBLE', ArmyId, ArmyPid});	
					 true ->
						 ok
				 end
		 end,
	
	lists:foreach(F2, ObservedByCandidateList).
					 	
calc_distance(ArmyX, ArmyY, EntityPid) ->
	EntityState = gen_server:call(EntityPid, {'GET_STATE', 1}),
	DiffX = ArmyX - EntityState#state.x,
	DiffY = ArmyY - EntityState#state.y,
	DiffX * DiffX + DiffY * DiffY.

get_army_speed(ArmyId) ->
    %UnitsSpeed = unit:units_speed(ArmyId),
    5.
    %lists:max(UnitsSpeed).

speed_to_ticks(Speed) ->
    Speed * (1000 div ?GAME_LOOP_TICK).

event_move(Army, NewX, NewY) ->
    io:fwrite("army - db_event_move~n"),
	Army#army{x = NewX,
              y = NewY}.  

state_move(Army, DestX, DestY) ->
    Army#army{dest_x = DestX, 
    	      dest_y = DestY,
              state = ?STATE_MOVE}.


state_attack(Army, TargetId) ->
    Army#army{state = ?STATE_ATTACK,
              target = TargetId}.


state_combat(Army, BattleId) ->
    Army#army{state = ?STATE_COMBAT,
			  battle = BattleId}.

state_combat(Army, BattleId, X, Y) ->
	Army#army{state = ?STATE_COMBAT,
			  battle = BattleId,
			  x = X,
              y = Y}.

state_none(Army, X, Y) ->
    Army#army{state = ?STATE_NONE,
              x = X,
              y = Y}.

state_none(Army) ->
    Army#army{state = ?STATE_NONE}. 

get_army_status(Data) ->
	
	NumUnits = dict:size(Data#module_data.units),
	
	if
		NumUnits =:= 0 ->
			ArmyStatus = ?ARMY_DEAD;
		true ->
			ArmyStatus = ?ARMY_ALIVE
	end,

	ArmyStatus.
						 
						
						
						

			
			
    