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

-export([create/3, start/1, setup/3, stop/1, add_army/2, remove_army/2]).

-record(module_data, {players,
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

init([BattleId, X, Y])
  when is_integer(BattleId) ->
    process_flag(trap_exit, true),
    
    Players = sets:new(),
    LastEvents = sets:new(),
    BattlePid = self(),
    Battle = #battle { id = BattleId,
                       armies = [],
                       x = X,
                       y = Y},    

    db:dirty_write(Battle),
    gen_server:cast(global:whereis_name(game_pid), {'ADD_BATTLE', BattlePid}),
    
    {ok, #module_data{players = Players, 
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
    io:fwrite("battle - battle_id: ~w~n", [BattleId]),
    
    Battle = db:read(battle, BattleId),
    Players = sets:new(),
    LastEvents = sets:new(),

    {ok, #module_data{players = Players, 
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
    
    NewData = add_army(AttackerPlayerId, AttackerId, add_army(DefenderPlayerId, DefenderId, Data)),
    
    send_info(NewData#module_data.battle_id, AttackerPlayerId, NewData#module_data.armies),
    send_info(NewData#module_data.battle_id, DefenderPlayerId, NewData#module_data.armies),
    
    {noreply, NewData};

handle_cast({'ADD_ARMY', ArmyId}, Data) ->
    io:fwrite("Battle ~w - Adding Army~n", [Data#module_data.battle_id]),
    PlayerId = gen_server:call(global:whereis_name({army, ArmyId}), {'GET_PLAYER_ID'}),
    BattleId = Data#module_data.battle_id,
    NewData = add_army(PlayerId, ArmyId, Data),
    
    send_info(BattleId, PlayerId, NewData#module_data.armies),
    broadcast_army_event(BattleId, NewData#module_data.players, PlayerId, ArmyId, add_army),
    {noreply, NewData};

handle_cast({'REMOVE_ARMY', ArmyId}, Data) ->
    io:fwrite("Battle Data ~w~n", [Data]),
    io:fwrite("Battle ~w - Removing Army~n", [Data#module_data.battle_id]),
    PlayerId = gen_server:call(global:whereis_name({army, ArmyId}), {'GET_PLAYER_ID'}),
    BattleId = Data#module_data.battle_id,
    NewData = remove_army(PlayerId, ArmyId, Data),
    
    send_info(BattleId, PlayerId, NewData#module_data.armies),
    broadcast_army_event(BattleId, NewData#module_data.players, PlayerId, ArmyId, remove_army),
    {noreply, NewData};  

handle_cast({'PROCESS_EVENT', EventTick, EventData, EventType}, Data) ->
    
    case EventType of
        ?EVENT_UNIT_ATTACK ->
            NewData = unit_attack(EventTick, EventData, Data),           
            io:fwrite("battle: ~w~n", [EventData]);
        ?EVENT_RETREAT ->
            retreat_move(EventData),
            NewData = Data;
        ?EVENT_LEAVE ->
            leave_move(EventData),
            NewData = Data;
        ?EVENT_NONE ->
            NewData = Data
    end,      
    
    {noreply, NewData};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({'ADD_TARGET', SourceArmyId, SourceUnitId, TargetArmyId, TargetUnitId}, _From, Data) ->
    
    SourceArmyResult = lists:member(SourceArmyId, Data#module_data.armies),
    TargetArmyResult = lists:member(TargetArmyId, Data#module_data.armies),
    
    GuardArmy = SourceArmyResult and TargetArmyResult,
    
    if
        GuardArmy ->
            SourceUnit = gen_server:call(global:whereis_name({army, SourceArmyId}), {'GET_UNIT', SourceUnitId}),
            TargetUnit = gen_server:call(global:whereis_name({army, TargetArmyId}), {'GET_UNIT', TargetUnitId}),
            io:fwrite("SourceUnit: ~w TargetUnit: ~w~n", [SourceUnit, TargetUnit]),
            GuardUnit = (SourceUnit =/= false) and (TargetUnit =/= false),
            
            if
                GuardUnit ->			
                    Targets = Data#module_data.targets,
                    NewTarget = #target{ source_army = SourceArmyId,
                                         source_unit = SourceUnitId,
                                         target_army = TargetArmyId,
                                         target_unit = TargetUnitId},
                    NewTargets = [NewTarget | Targets],
                    NewData = Data#module_data { targets = NewTargets},
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

%% RETREAT AND LEAVE ARE SIMILAR FOR NOW %%
handle_call({'RETREAT', SourceArmyId}, _From, Data) ->
    log4erl:info("~w: RETREAT received", [?MODULE]),
    case lists:member(SourceArmyId, Data#module_data.armies) of
        true ->
            ArmyPid = global:whereis_name({army, SourceArmyId}),
            EventTime = unit:calc_retreat_time(SourceArmyId) * trunc(1000 / ?GAME_LOOP_TICK),
            EventData = SourceArmyId,

            gen_server:cast(ArmyPid, {'SET_STATE_RETREAT', Data#module_data.battle_id}),                        
            game:add_event(Data#module_data.self, ?EVENT_RETREAT, EventData, EventTime),
            NewEvents = clear_events(SourceArmyId, Data#module_data.events, []),
            NewData = Data#module_data { events = NewEvents},

            RetreatInfo = {battle_retreat, success};
        false ->
            NewData = Data,
            RetreatInfo = {battle_retreat, invalid_army}
    end,
    
    {reply, RetreatInfo, NewData};     

handle_call({'LEAVE', SourceArmyId}, _From, Data) ->
    log4erl:info("~w: LEAVE received", [?MODULE]),
    case lists:member(SourceArmyId, Data#module_data.armies) of
        true ->
            ArmyPid = global:whereis_name({army, SourceArmyId}),
            EventTime = unit:calc_leave_time(SourceArmyId) * trunc(1000 / ?GAME_LOOP_TICK),
            EventData = SourceArmyId,

            gen_server:cast(ArmyPid, {'SET_STATE_LEAVE', Data#module_data.battle_id}),
            game:add_event(Data#module_data.self, ?EVENT_LEAVE, EventData, EventTime),
            NewEvents = clear_events(SourceArmyId, Data#module_data.events, []),
            NewData = Data#module_data { events = NewEvents},
            
            LeaveInfo = {battle_leave, success};
        false ->
            NewData = Data,
            LeaveInfo = {battle_leave, invalid_army}
    end,

    {reply, LeaveInfo, NewData};
    
handle_call({'GET_STATE', _BattleId}, _From, Data) ->
    
    State = #state { id = Data#module_data.battle_id,
                     player_id = ?PLAYER_NONE,
                     type = ?OBJECT_BATTLE,
                     state = ?STATE_NONE,
                     x = Data#module_data.x,
                     y = Data#module_data.y},
    
    {reply, State, Data};

handle_call({'GET_INFO', PlayerId}, _From, Data) ->
    
    Result = sets:is_element(PlayerId, Data#module_data.players),
    
    if
        Result ->
            ArmiesInfoTuple = armies_tuple(Data#module_data.armies, []),
            BattleInfo = {detailed, Data#module_data.battle_id, ArmiesInfoTuple};
        true ->
            BattleInfo = {generic, Data#module_data.battle_id}
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

add_army(PlayerId, ArmyId, Data) ->	
    io:fwrite("Battle - add_army~n"),
    ArmyUnits = gen_server:call(global:whereis_name({army, ArmyId}), {'GET_UNITS'}),
    
    NewEvents = add_events(ArmyUnits, ArmyId, Data#module_data.self, Data#module_data.events),
    
    NewArmies = [ArmyId | Data#module_data.armies],
    NewPlayers = sets:add_element(PlayerId, Data#module_data.players),
    
    Data#module_data {armies = NewArmies, players = NewPlayers, events = NewEvents}.

add_events([], _, _, Events) ->
    io:fwrite("Battle - Finished Adding Events~n"),
    Events;

add_events(Units, ArmyId, BattlePid, Events) ->    
    io:fwrite("Battle - add_events - Units: ~w~n", [Units]),    
    [Unit | Rest] = Units,   
    [UnitType] = db:dirty_read(unit_type, Unit#unit.type),
    UnitSpeed = UnitType#unit_type.speed,
    EventTime = UnitSpeed * trunc(1000 / ?GAME_LOOP_TICK),
    
    EventId = game:add_event_get_id(BattlePid, ?EVENT_UNIT_ATTACK, {ArmyId, Unit#unit.id, UnitSpeed}, EventTime),
    NewEvents = [{ArmyId, EventId, ?EVENT_UNIT_ATTACK} | Events],
    
    add_events(Rest, ArmyId, BattlePid, NewEvents).

remove_army(PlayerId, ArmyId, Data) ->
    NewArmies = lists:delete(ArmyId, Data#module_data.armies),
    NewPlayers = sets:del_element(PlayerId, Data#module_data.players),
    
    Data#module_data {armies = NewArmies, players = NewPlayers}.

unit_attack(EventTick, EventData, Data) ->
    
    {ArmyId, UnitId, UnitSpeed} = EventData,
    
    ArmyResult = lists:member(ArmyId, Data#module_data.armies),
    Unit = gen_server:call(global:whereis_name({army, ArmyId}), {'GET_UNIT', UnitId}),
    
    if
        ArmyResult ->
            if
                Unit =/= false ->
                    NewLastEvents = sets:add_element({ArmyId, EventTick}, Data#module_data.last_events),
                    LastEventsData = Data#module_data {last_events = NewLastEvents},
                    UnitTargetData = unit_target(ArmyId, Unit, LastEventsData),
                                        
                    EventTime = UnitSpeed * trunc(1000 / ?GAME_LOOP_TICK),    
                    EventId = game:add_event_get_id(Data#module_data.self, ?EVENT_UNIT_ATTACK, EventData, EventTime),
                    NewEvents = [{ArmyId, EventId, ?EVENT_UNIT_ATTACK} | UnitTargetData#module_data.events],
                    NewData = UnitTargetData#module_data {events = NewEvents};
                true ->
                    NewData = Data
            end;
        
        true ->
            NewData = Data
    end,
    NewData.

unit_target(ArmyId, Unit, Data) ->
    TargetResult = lists:keysearch(Unit#unit.id, 3, Data#module_data.targets),
    
    if
        TargetResult =/= false ->	
            {value, Target} = TargetResult,
            TargetArmyId = Target#target.target_army,
            TargetUnitId = Target#target.target_unit,
            
            %TODO Check TargetArmyId is in battle
            TargetUnit = gen_server:call(global:whereis_name({army, TargetArmyId}), {'GET_UNIT', TargetUnitId}),
            
            NewData = unit_damage(ArmyId, Unit, TargetArmyId, TargetUnit, Data);                       
        true ->
            NewData = Data
    end,
    
    NewData.

unit_damage(_ArmyId, Unit, TargetArmyId, TargetUnit, Data) ->
    
    Guard = (Unit =/= false) and (TargetUnit =/= false),
    
    if
        Guard ->
            
            io:fwrite("Battle - Apply Unit Damage.~n"),
            
            [UnitType] = db:dirty_read(unit_type, Unit#unit.type),
            [_TargetUnitType] = db:dirty_read(unit_type, TargetUnit#unit.type),
            Damage = UnitType#unit_type.attack,
            
            ArmyState = gen_server:call(global:whereis_name({army, TargetArmyId}), {'DAMAGE_UNIT', TargetUnit#unit.id, Damage}),
            
            broadcast_damage(Data#module_data.battle_id, Data#module_data.players, Unit#unit.id, TargetUnit#unit.id, Damage),
            
            if
                ArmyState =:= ?STATE_DEAD ->                    
                    io:fwrite("Battle - Army ~w destroyed.~n", [TargetArmyId]),
                    Armies = Data#module_data.armies,
                    NewArmies = lists:delete(TargetArmyId, Armies),
                    NewData = Data#module_data {armies = NewArmies};
                true ->
                    NewData = Data
            end;        	
        true ->
            NewData = Data
    end,
    NewData.

send_info(BattleId, PlayerId, Armies) ->
    case gen_server:call(global:whereis_name(game_pid), {'IS_PLAYER_ONLINE', PlayerId}) of
        true ->
            ArmiesInfoTuple = armies_tuple(Armies, []),
            gen_server:cast(global:whereis_name({player, PlayerId}), {'SEND_BATTLE_INFO', BattleId, ArmiesInfoTuple});
        false ->
            ok
    end.	

send_damage(BattleId, PlayerId, SourceId, TargetId, Damage) ->
    case gen_server:call(global:whereis_name(game_pid), {'IS_PLAYER_ONLINE', PlayerId}) of
        true ->
            gen_server:cast(global:whereis_name({player, PlayerId}), {'SEND_BATTLE_DAMAGE', BattleId, SourceId, TargetId, Damage});
        false ->
            ok
    end.

broadcast_damage(BattleId, Players, SourceId, TargetId, Damage) ->
    PlayersList = sets:to_list(Players),
    
    F = fun(PlayerId) ->
                send_damage(BattleId, PlayerId, SourceId, TargetId, Damage)
        end,
    
    lists:foreach(F, PlayersList).

broadcast_army_event(BattleId, Players, NewPlayerId, ArmyId, ArmyEvent) ->
    PlayersList = sets:to_list(Players),
    
    F = fun(PlayerId) ->
                if
                    NewPlayerId =/= PlayerId ->
                        case gen_server:call(global:whereis_name(game_pid), {'IS_PLAYER_ONLINE', PlayerId}) of
                            true ->
                                ArmyInfo = gen_server:call(global:whereis_name({army, ArmyId}), {'GET_INFO'}),
                                case ArmyEvent of
                                    {add_army} ->
                                        player:send_battle_add_army(PlayerId, BattleId, ArmyInfo);
                                    {remove_army} ->
                                        player:send_battle_remove_army(PlayerId, BattleId, ArmyInfo);
                                    _ ->
                                        ok
                                end;
                            false ->
                                ok
                        end;
                    true ->
                        ok
                end
        end,
    
    lists:foreach(F, PlayersList).

armies_tuple([], ArmiesInfoTuple) ->
    ArmiesInfoTuple;

armies_tuple(Armies, ArmiesInfoTuple) ->
    [ArmyId | Rest] = Armies,
    ArmyInfo = gen_server:call(global:whereis_name({army, ArmyId}), {'GET_INFO'}),
    NewArmiesInfoTuple = [ArmyInfo | ArmiesInfoTuple],
    armies_tuple(Rest, NewArmiesInfoTuple).

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

leave_move(ArmyId) ->
    gen_server:cast(global:whereis_name({army, ArmyId}), {'SET_STATE_LEAVE_MOVE'}).
    
