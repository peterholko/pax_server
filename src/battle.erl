%% Author: Peter
%% Created: Feb 4, 2009
%% Description: TODO: Add description to army
-module(battle).
-behaviour(gen_server).

%%
%% Include files
%%

-include("common.hrl").
-include("schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

%%
%% Exported Functions
%%
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-export([create/1, start/1, stop/1]).

-record(module_data, {players,
					  armies,
					  targets,
					  battle_id,
					  self}).

-record(target, {source_army,
				 source_unit,
				 target_army,
				 target_unit}).

%%
%% API Functions
%%

create(BattleId) ->
	start(BattleId).

start(BattleId) ->
	gen_server:start({global, {battle, BattleId}}, battle, [BattleId], []).

init([BattleId]) 
  when is_integer(BattleId) ->
    process_flag(trap_exit, true),
    io:fwrite("battle - battle_id: ~w~n", [BattleId]),
	
    Players = sets:new(),
    {ok, #module_data{players = Players, armies = [], targets = [], battle_id = BattleId, self = self()}}.

terminate(_Reason, _) ->
    ok.

stop(ProcessId) 
  when is_pid(ProcessId) ->
    gen_server:cast(ProcessId, stop).

handle_cast({'SETUP', AttackerId, DefenderId}, Data) ->
	
	AttackerPlayerId = gen_server:call(global:whereis_name({army, AttackerId}), {'GET_PLAYER_ID'}),
	DefenderPlayerId = gen_server:call(global:whereis_name({army, DefenderId}), {'GET_PLAYER_ID'}),	
	
	NewData = add_army(AttackerPlayerId, AttackerId, add_army(DefenderPlayerId, DefenderId, Data)),
	
	send_joined(NewData#module_data.battle_id, AttackerPlayerId, NewData#module_data.armies),
	send_joined(NewData#module_data.battle_id, DefenderPlayerId, NewData#module_data.armies),

	{noreply, NewData};

handle_cast({'ADD_ARMY', ArmyId}, Data) ->
    io:fwrite("Battle ~w - Adding Army~n", [Data#module_data.battle_id]),
	PlayerId = gen_server:call(global:whereis_name({army, ArmyId}), {'GET_PLAYER_ID'}),
	NewData = add_army(PlayerId, ArmyId, Data),
	
	send_joined(NewData#module_data.battle_id, PlayerId, NewData#module_data.armies),
	broadcast_add_army(NewData#module_data.battle_id, NewData#module_data.players, PlayerId, ArmyId),
		
	{noreply, NewData};

handle_cast({'PROCESS_EVENT', EventData, EventType}, Data) ->
    
    case EventType of
        ?EVENT_UNIT_ROUND ->
                        
            NewData = unit_round(EventData, Data),
    
            io:fwrite("battle: ~w~n", [EventData]);
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
    ArmyUnitsIds = dict:fetch_keys(ArmyUnits),

	add_events(ArmyUnitsIds, ArmyId, Data#module_data.self),
	
    NewArmies = [ArmyId | Data#module_data.armies],
	NewPlayers = sets:add_element(PlayerId, Data#module_data.players),
	
	Data#module_data {armies = NewArmies, players = NewPlayers}.

add_events([], _, _) ->
    io:fwrite("Battle - Finished Adding Events~n"),
  	ok;

add_events(UnitsIds, ArmyId, BattlePid) ->
    
    io:fwrite("Battle - add_events - UnitsIds: ~w~n", [UnitsIds]),
    
	[UnitId | Rest] = UnitsIds,
    
    Unit = gen_server:call(global:whereis_name({army, ArmyId}), {'GET_UNIT', UnitId}),
        
    [UnitType] = mnesia:dirty_read(unit_type, Unit#unit.type),
    UnitSpeed = UnitType#unit_type.speed,
    EventTime = UnitSpeed * trunc(1000 / ?GAME_LOOP_TICK),
    
    gen_server:cast(global:whereis_name(game_pid), {'ADD_EVENT', BattlePid, ?EVENT_UNIT_ROUND, {ArmyId, Unit#unit.id, UnitSpeed}, EventTime}),
    
	add_events(Rest, ArmyId, BattlePid).

unit_round(EventData, Data) ->
    
    {ArmyId, UnitId, UnitSpeed} = EventData,
        
    ArmyResult = lists:member(ArmyId, Data#module_data.armies),
	Unit = gen_server:call(global:whereis_name({army, ArmyId}), {'GET_UNIT', UnitId}),
    	
    if
        ArmyResult ->
            if
				Unit =/= false ->
					
					NewData = unit_target(ArmyId, Unit, Data),
					
					EventTime = UnitSpeed * trunc(1000 / ?GAME_LOOP_TICK),    
					gen_server:cast(global:whereis_name(game_pid), {'ADD_EVENT', Data#module_data.self, ?EVENT_UNIT_ROUND, EventData, EventTime});
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

        NewData = unit_attack(ArmyId, Unit, TargetArmyId, TargetUnit, Data);                       
    true ->
		NewData = Data
    end,
	
	NewData.

unit_attack(ArmyId, Unit, TargetArmyId, TargetUnit, Data) ->
    
    Guard = (Unit =/= false) and (TargetUnit =/= false),
    
    if
        Guard ->
			
			io:fwrite("Battle - Apply Unit Damage.~n"),
			
            [UnitType] = db:dirty_read(unit_type, Unit#unit.type),
			[TargetUnitType] = db:dirty_read(unit_type, TargetUnit#unit.type),
            Damage = UnitType#unit_type.attack,
			
            ArmyStatus = gen_server:call(global:whereis_name({army, TargetArmyId}), {'DAMAGE_UNIT', TargetUnit#unit.id, Damage}),
			PlayerId = gen_server:call(global:whereis_name({army, ArmyId}), {'GET_PLAYER_ID'}),
				
			send_damage(Data#module_data.battle_id, PlayerId, Unit#unit.id, TargetUnit#unit.id, Damage),
			
			if
				ArmyStatus =:= ?ARMY_DEAD ->
					
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

send_joined(BattleId, PlayerId, Armies) ->
	case gen_server:call(global:whereis_name(game_pid), {'IS_PLAYER_ONLINE', PlayerId}) of
		true ->
			ArmiesInfoTuple = armies_tuple(Armies, []),
			gen_server:cast(global:whereis_name({player, PlayerId}), {'SEND_BATTLE_JOINED', BattleId, ArmiesInfoTuple});
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

broadcast_add_army(BattleId, Players, NewPlayerId, ArmyId) ->
	PlayersList = sets:to_list(Players),
	
	F = fun(PlayerId) ->
				if
					NewPlayerId =/= PlayerId ->
						case gen_server:call(global:whereis_name(game_pid), {'IS_PLAYER_ONLINE', PlayerId}) of
							true ->
								ArmyInfo = gen_server:call(global:whereis_name({army, ArmyId}), {'GET_INFO'}),
								gen_server:cast(global:whereis_name({player, PlayerId}), {'SEND_BATTLE_ADD_ARMY', BattleId, ArmyInfo});
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
	
	
									  