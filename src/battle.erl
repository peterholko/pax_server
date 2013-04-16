%% Author: Peter
%% Created: Feb 4, 2009
%% Description: TODO: Add description to army
-module(battle).
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

-export([create/3, start/1, setup/3, stop/1, add_army/2, remove_army/2, retreat/2]).

-record(data, {players,
                      armies,
                      targets,
                      battle_id,
                      x,
                      y,
                      events,
                      last_events,
                      self}).

-record(target, {source_army,
                 source_unit,
                 target_army,
                 target_unit}).

%%
%% API Functions
%%

create(BattleId, X, Y) ->
    gen_server:start({global, {battle, BattleId}}, battle, [BattleId, X, Y], []).

start(BattleId) ->
    gen_server:start({global, {battle, BattleId}}, battle, [BattleId], []).

setup(BattleId, AttackerId, DefenderId) ->
    gen_server:cast(global:whereis_name({battle, BattleId}), {'SETUP', AttackerId, DefenderId}).

add_army(BattleId, ArmyId) ->
    gen_server:cast(global:whereis_name({battle, BattleId}), {'ADD_ARMY', ArmyId}).

remove_army(BattleId, ArmyId) ->
    gen_server:cast(global:whereis_name({battle, BattleId}), {'REMOVE_ARMY', ArmyId}).

retreat(BattleId, ArmyId) ->
    gen_server:call(global:whereis_name({battle, BattleId}), {'RETREAT', ArmyId}).

init([BattleId, X, Y])
  when is_integer(BattleId) ->
    process_flag(trap_exit, true),
    ?INFO("Initializing new battle"),
    Players = sets:new(),
    LastEvents = sets:new(),
    BattlePid = self(),
    Battle = #battle { id = BattleId,
                       x = X,
                       y = Y},
    db:dirty_write(Battle),
    
    %Create map object for the battle
    map_object:create(BattleId, ?OBJECT_BATTLE, X, Y),   
 
    {ok, #data{players = Players, 
                      armies = [], 
                      targets = [], 
                      battle_id = BattleId, 
                      x = X, 
                      y = Y, 
                      events = [],
                      last_events = LastEvents, 
                      self = BattlePid}};	

init([BattleId]) 
  when is_integer(BattleId) ->
    process_flag(trap_exit, true),
    ?INFO("Initializing existing battle"),
    
    Battle = db:read(battle, BattleId),
    Players = sets:new(),
    LastEvents = sets:new(),

    {ok, #data{players = Players, 
                      armies = Battle#battle.armies, 
                      targets = [], 
                      battle_id = BattleId, 
                      events = [],
                      last_events = LastEvents,
                      self = self()}}.

terminate(_Reason, _) ->
    ok.

stop(ProcessId) 
  when is_pid(ProcessId) ->
    gen_server:cast(ProcessId, stop).

%%
%% OTP handlers
%%

handle_cast({'SETUP', AttackerId, DefenderId}, Data) ->
    
    AttackerPlayerId = gen_server:call(global:whereis_name({army, AttackerId}), {'GET_PLAYER_ID'}),
    DefenderPlayerId = gen_server:call(global:whereis_name({army, DefenderId}), {'GET_PLAYER_ID'}),	
    
    NewData = new_army(AttackerPlayerId, AttackerId, new_army(DefenderPlayerId, DefenderId, Data)),
    
    send_info(NewData#data.battle_id, AttackerPlayerId, NewData#data.armies),
    send_info(NewData#data.battle_id, DefenderPlayerId, NewData#data.armies),
    
    {noreply, NewData};

handle_cast({'ADD_ARMY', ArmyId}, Data) ->
    io:fwrite("Battle ~w - Adding Army~n", [Data#data.battle_id]),
    ?INFO("BattleId: ", Data#data.battle_id, "Adding army: ", ArmyId),

    PlayerId = entity:player_id(ArmyId),
    BattleId = Data#data.battle_id,
    NewData = new_army(PlayerId, ArmyId, Data),
   
    broadcast_army_event(BattleId, NewData#data.players, ArmyId, ?BATTLE_ADD_ARMY),

    {noreply, NewData};

handle_cast({'REMOVE_ARMY', ArmyId}, Data) ->
    PlayerId = gen_server:call(global:whereis_name({army, ArmyId}), {'GET_PLAYER_ID'}),
    BattleId = Data#data.battle_id,
    NewData = remove_army(PlayerId, ArmyId, Data),
    
    broadcast_army_event(BattleId, NewData#data.players, ArmyId, ?BATTLE_REMOVE_ARMY),
    {noreply, NewData};  

handle_cast({'PROCESS_EVENT', EventTick, EventData, EventType}, Data) ->
    
    case EventType of
        ?EVENT_UNIT_ATTACK ->
            NewData = unit_attack(EventTick, EventData, Data);
        ?EVENT_RETREAT ->
            ArmyId = EventData,
            retreat_move(ArmyId),

            %Broadcast army state to everyone
            broadcast_army_event(Data#data.battle_id, 
                                 Data#data.players, 
                                 ArmyId, 
                                 ?BATTLE_MOVE),

            NewData = Data;
        ?EVENT_NONE ->
            NewData = Data
    end,      
    
    {noreply, NewData};

handle_cast({'ADD_VISIBLE', _BattleId, _EntityId, _EntityPid}, Data) ->    
    %Do nothing atm 
    {noreply, Data};

handle_cast({'REMOVE_VISIBLE', _BattleId, _EntityId, _EntityPid}, Data) ->
    %Do nothing atm	
    {noreply, Data};

handle_cast({'ADD_OBSERVED_BY', BattleId, EntityId, EntityPid}, Data) ->    
    
    EntityType = gen_server:call(EntityPid, {'GET_TYPE', EntityId}),
        
    case EntityType of
        ?OBJECT_ARMY ->
            add_observed_by(BattleId, EntityId, EntityPid);
        ?OBJECT_CITY ->
            add_observed_by(BattleId, EntityId, EntityPid)
    end, 

    {noreply, Data};

handle_cast({'REMOVE_OBSERVED_BY', BattleId, EntityId, EntityPid}, Data) ->    
    
    remove_observed_by(BattleId, EntityId, EntityPid),
    
    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({'ADD_TARGET', SourceArmyId, SourceUnitId, TargetArmyId, TargetUnitId}, _From, Data) ->
    
    SourceArmyResult = lists:member(SourceArmyId, Data#data.armies),
    TargetArmyResult = lists:member(TargetArmyId, Data#data.armies),
    
    GuardArmy = SourceArmyResult and TargetArmyResult,
    
    if
        GuardArmy ->
            SourceUnit = unit:get_unit(SourceUnitId),
            TargetUnit = unit:get_unit(TargetUnitId),
            ?INFO("SourceUnit: ", SourceUnit, "TargetUnit: ", TargetUnit),
            GuardUnit = (SourceUnit =/= false) and (TargetUnit =/= false),
            
            if
                GuardUnit ->			
                    Targets = Data#data.targets,
                    NewTarget = #target{ source_army = SourceArmyId,
                                         source_unit = SourceUnitId,
                                         target_army = TargetArmyId,
                                         target_unit = TargetUnitId},
                    NewTargets = [NewTarget | Targets],
                    NewData = Data#data { targets = NewTargets},
                    TargetInfo = {battle_target, success};
                true ->
                    NewData = Data,
                    TargetInfo = {battle_target, invalid_unit}
            end;
        true ->
            NewData = Data,
            TargetInfo = {battle_target, invalid_army}
    end,
    
    {reply, TargetInfo, NewData};

handle_call({'RETREAT', SourceArmyId}, _From, Data) ->
    ?INFO("RETREAT received"),
    case lists:member(SourceArmyId, Data#data.armies) of
        true ->
    
            ArmyPid = global:whereis_name({army, SourceArmyId}),
            EventTime = unit:calc_retreat_time(SourceArmyId) * trunc(1000 / ?GAME_LOOP_TICK),
            EventData = SourceArmyId,

            %Set Army to RETREAT state
            gen_server:cast(ArmyPid, {'SET_STATE_RETREAT', Data#data.battle_id}),

            %Create RETREAT game event                        
            game:add_event(Data#data.self, ?EVENT_RETREAT, EventData, EventTime),

            %Clear unit events
            NewEvents = clear_events(SourceArmyId, Data#data.events, []),
            NewData = Data#data { events = NewEvents},
    
            %Do not broadcast the retreat state to everyone
            %broadcast_army_event(NewData#data.battle_id, 
            %                     NewData#data.players, 
            %                     SourceArmyId, 
            %                     ?BATTLE_RETREAT),

            RetreatInfo = {battle_retreat, success};
        false ->
            NewData = Data,
            RetreatInfo = {battle_retreat, invalid_army}
    end,
    
    {reply, RetreatInfo, NewData};     

handle_call({'GET_STATE', _BattleId}, _From, Data) ->
    
    State = #state { id = Data#data.battle_id,
                     player_id = ?PLAYER_NONE,
                     type = ?OBJECT_BATTLE,
                     subtype = ?OBJECT_BASIC,
                     state = ?STATE_NONE,
                     x = Data#data.x,
                     y = Data#data.y},
    
    {reply, State, Data};

handle_call({'GET_INFO', PlayerId}, _From, Data) ->
    Result = sets:is_element(PlayerId, Data#data.players),
    ?INFO("GET_INFO: ", PlayerId, " ", Result),
    ?INFO("Players: ", Data#data.players),
    
    if
        Result ->
            %Convert army records to tuple packet form
            ArmiesInfoTuple = armies_tuple(Data#data.armies, []),

            %Convert items record to tuple packet form
            BattlePlayerId = -1,
            Items = item:get_by_entity({?OBJECT_BATTLE, Data#data.battle_id}, BattlePlayerId),
            ItemsTuple = item:tuple_form(Items),
            
            BattleInfo = {detailed, Data#data.battle_id, ArmiesInfoTuple, ItemsTuple};
        true ->
            BattleInfo = {generic, Data#data.battle_id}
    end,
    
    {reply, BattleInfo, Data};

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

new_army(PlayerId, ArmyId, Data) ->	
    ?INFO("new_army: ", {PlayerId, ArmyId}),
    
    %TODO use unit module
    ArmyUnits = gen_server:call(global:whereis_name({army, ArmyId}), {'GET_UNITS'}),
    
    NewEvents = add_events(ArmyUnits, ArmyId, Data#data.self, Data#data.events),
    
    NewArmies = [ArmyId | Data#data.armies],
    NewPlayers = sets:add_element(PlayerId, Data#data.players),
    
    Data#data {armies = NewArmies, players = NewPlayers, events = NewEvents}.

add_events([], _, _, Events) ->
    ?INFO("add_events finished"),
    Events;

add_events([Unit | Rest], ArmyId, BattlePid, Events) ->   
    ?INFO("add_events unit: ", Unit), 
    Template = unit:get_template(Unit),
    UnitSpeed = unit:get_speed(Template),
    EventTime = UnitSpeed * trunc(1000 / ?GAME_LOOP_TICK),

    EventId = game:add_event_get_id(BattlePid, ?EVENT_UNIT_ATTACK, {ArmyId, Unit#unit.id, UnitSpeed}, EventTime),
    NewEvents = [{ArmyId, EventId, ?EVENT_UNIT_ATTACK} | Events],
    
    add_events(Rest, ArmyId, BattlePid, NewEvents).

remove_army(PlayerId, ArmyId, Data) ->
    NewArmies = lists:delete(ArmyId, Data#data.armies),
    NewPlayers = sets:del_element(PlayerId, Data#data.players),
    
    Data#data {armies = NewArmies, players = NewPlayers}.

unit_attack(EventTick, EventData, Data) ->
    {ArmyId, UnitId, UnitSpeed} = EventData,
    
    ArmyResult = lists:member(ArmyId, Data#data.armies),
    ?INFO("unit_attack Get Unit"),
    Unit = unit:get_unit(UnitId),
    
    if
        ArmyResult ->
            if
                Unit =/= false ->
                    NewLastEvents = sets:add_element({ArmyId, EventTick}, Data#data.last_events),
                    LastEventsData = Data#data {last_events = NewLastEvents},
                    UnitTargetData = unit_target(ArmyId, Unit, LastEventsData),
                                        
                    EventTime = UnitSpeed * trunc(1000 / ?GAME_LOOP_TICK),    
                    EventId = game:add_event_get_id(Data#data.self, ?EVENT_UNIT_ATTACK, EventData, EventTime),
                    NewEvents = [{ArmyId, EventId, ?EVENT_UNIT_ATTACK} | UnitTargetData#data.events],
                    NewData = UnitTargetData#data {events = NewEvents};
                true ->
                    NewData = Data
            end;
        
        true ->
            NewData = Data
    end,
    NewData.

unit_target(ArmyId, Unit, Data) ->
%    TargetResult = lists:keysearch(Unit#unit.id, 3, Data#data.targets),  
%    if
%        TargetResult =/= false ->	
%            {value, Target} = TargetResult,
%            TargetArmyId = Target#target.target_army,
%            TargetUnitId = Target#target.target_unit,
    TargetArmies = lists:delete(ArmyId, Data#data.armies),
    if
        length(TargetArmies) > 0 ->
            {TargetUnitId, TargetArmyId} = best_target(TargetArmies),                         
            ?INFO("unit_target Get Unit"),
            TargetUnit = unit:get_unit(TargetUnitId),
            
            NewData = unit_damage(ArmyId, Unit, TargetArmyId, TargetUnit, Data);                       
        true ->
            NewData = Data
    end,
    
    NewData.

best_target(TargetArmies) ->
    ?INFO("TargetArmies: ", TargetArmies),
    UnitDPSList = unit_dps_list(TargetArmies, []),
    ?INFO("UnitDPSList: ", UnitDPSList),
    {_MaxDPS, UnitId, NewArmyId} = lists:max(UnitDPSList),
    {UnitId, NewArmyId}.

unit_dps_list([], UnitList) ->
    UnitList;
        
unit_dps_list([ArmyId | Rest], UnitList) ->
    Units = unit:get_units(ArmyId),
    
    F = fun(Unit, UnitDPSList) ->            
            Template = unit:get_template(Unit),
            Speed = unit:get_speed(Template),
            Damage = unit:get_attack(Template),
            Size = Unit#unit.size,
            DPS = Damage * Size / Speed,
            [{DPS, Unit#unit.id, ArmyId} | UnitDPSList]
        end,

    NewUnitList = lists:foldl(F, [], Units) ++ UnitList,
    unit_dps_list(Rest, NewUnitList).

unit_damage(ArmyId, Unit, TargetArmyId, TargetUnit, Data) ->
    ?INFO("Applying damage to unit: ", TargetUnit#unit.id), 

    case unit:damage(Data#data.battle_id, Unit, TargetUnit) of
        {army_destroyed, Damage} ->
            ?INFO("Army destroyed: ", TargetArmyId),
            unit_destroyed(Unit#unit.id),
            army:destroyed(TargetArmyId),

            %TargetPlayerId = entity:player_id(TargetArmyId),

            NewArmies = lists:delete(TargetArmyId, Data#data.armies),
            %Do not remove player as they will not be able to open the battle 
            %NewPlayers = sets:del_element(TargetPlayerId, Data#data.players),
            
            NewData = Data#data {players = Data#data.players,
                                 armies = NewArmies};
        {unit_destroyed, Damage} ->
            ?INFO("Unit destroyed"),
            unit_destroyed(Unit#unit.id),

            NewData = Data;
        {unit_damaged, Damage} ->
            ?INFO("Unit damaged"),
            NewData = Data
    end,

    broadcast_damage(Data#data.battle_id, 
                     Data#data.players,
                     ArmyId,    
                     Unit#unit.id, 
                     TargetArmyId,
                     TargetUnit#unit.id, 
                     trunc(Damage)),

    %TODO Do not send broadcast_info on unit_damage, only on army and unit destroyed 
    broadcast_info(Data#data.battle_id, Data#data.players, NewData#data.armies),
    NewData.

send_info(BattleId, PlayerId, Armies) ->
    case gen_server:call(global:whereis_name(game_pid), {'IS_PLAYER_ONLINE', PlayerId}) of
        true ->
            %Convert army records to tuple packet form
            ArmiesInfoTuple = armies_tuple(Armies, []),

            %Convert items record to tuple packet form
            BattlePlayerId = -1,
            Items = item:get_by_entity({?OBJECT_BATTLE, BattleId}, BattlePlayerId),
            ItemsTuple = item:tuple_form(Items),

            gen_server:cast(global:whereis_name({player, PlayerId}), {'SEND_BATTLE_INFO', BattleId, ArmiesInfoTuple, ItemsTuple});
        false ->
            ok
    end.	

send_damage(BattleId, PlayerId, Source, Target, Damage) ->
    case gen_server:call(global:whereis_name(game_pid), {'IS_PLAYER_ONLINE', PlayerId}) of
        true ->
            player:send_battle_damage(PlayerId, BattleId, Source, Target, Damage);
        false ->
            ok
    end.

broadcast_damage(BattleId, Players, SourceArmyId, SourceUnitId, 
                 TargetArmyId, TargetUnitId, Damage) ->
    PlayersList = sets:to_list(Players),

    Source = {SourceArmyId, SourceUnitId},
    Target = {TargetArmyId, TargetUnitId},
    
    F = fun(PlayerId) ->
            send_damage(BattleId, PlayerId, Source, Target, Damage)
        end,
    
    lists:foreach(F, PlayersList).

broadcast_info(BattleId, Players, Armies) ->
    PlayersList = sets:to_list(Players),

    F = fun(PlayerId) ->
            send_info(BattleId, PlayerId, Armies)
        end,

    lists:foreach(F, PlayersList).

broadcast_army_event(BattleId, Players, ArmyId, BattleEvent) ->
    PlayersList = sets:to_list(Players),
    F = fun(PlayerId) ->
        case gen_server:call(global:whereis_name(game_pid), {'IS_PLAYER_ONLINE', PlayerId}) of
            true ->
                ArmyInfo = army:battle_get_info(ArmyId),
                player:send_battle_event(PlayerId, BattleEvent, BattleId, ArmyInfo);
            false ->
                ok
        end    
    end,
    
    lists:foreach(F, PlayersList).

   
armies_tuple([], ArmiesInfoTuple) ->
    ArmiesInfoTuple;

armies_tuple(Armies, ArmiesInfoTuple) ->
    [ArmyId | Rest] = Armies,
    ArmyInfo = army:battle_get_info(ArmyId),
    NewArmiesInfoTuple = [ArmyInfo | ArmiesInfoTuple],
    armies_tuple(Rest, NewArmiesInfoTuple).

% Clear Unit Events 
clear_events(_ArmyId, [], NewEvents) ->
    NewEvents;    

clear_events(ArmyId, Events, LeftOverEvents) ->
    [Event | Rest] = Events,
    {EventArmyId, EventId, EventType} = Event,
    
    case EventType of 
        ?EVENT_UNIT_ATTACK ->
            case ArmyId =:= EventArmyId of
                true ->
                    game:delete_event(EventId),
                    NewLeftOverEvents = LeftOverEvents;
                false ->
                    NewLeftOverEvents = [Event | LeftOverEvents]
            end;
        _ ->
            NewLeftOverEvents = [Event | LeftOverEvents]
    end,
    
    clear_events(ArmyId, Rest, NewLeftOverEvents).

retreat_move(ArmyId) ->
    gen_server:cast(global:whereis_name({army, ArmyId}), {'SET_STATE_RETREAT_MOVE'}).

add_observed_by(BattleId, EntityId, EntityPid) ->
    [Battle] = db:dirty_read(battle, BattleId),
    NewObservedByList = [{EntityId, EntityPid} | Battle#battle.observed_by],
    NewBattle = Battle#battle { observed_by = NewObservedByList},
    db:dirty_write(NewBattle).

remove_observed_by(BattleId, EntityId, EntityPid) ->
    [Battle] = db:dirty_read(battle, BattleId),
    NewObservedByList = lists:delete({EntityId, EntityPid}, Battle#battle.observed_by),
    NewBattle = Battle#battle { observed_by = NewObservedByList},
    db:dirty_write(NewBattle).                                  

unit_destroyed(UnitId) ->
     game:delete_event(UnitId).
