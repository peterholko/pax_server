% Author: Peter
%% Created: Dec 24, 2008
%% Description: TODO: Add description to player
-module(player).
-behaviour(gen_server).

%%
%% Include files
%%

-include("game.hrl").
-include("packet.hrl").
-include("schema.hrl").

%%
%% Exported Functions
%%
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-export([start/1, stop/1, stop/2, get_explored_map/1, get_type/1]).

%%
%% Records
%%

-record(module_data, {
                      player_id,
                      socket = none,  		            
                      explored_map = [], 
                      discovered_tiles = [],  
                      update_perception = false,	    
                      self
                     }).

%%
%% API Functions
%%

get_type(PlayerId) ->
    [PlayerType] = db:read(player_type, PlayerId),
    PlayerType#player_type.type.

start(Name) 
  when is_binary(Name) ->
    
    %% make sure player exist
    case db:index_read(player, Name, #player.name) of
        [PlayerInfo] ->
            PlayerId = PlayerInfo#player.id,
            gen_server:start({global, {player, PlayerId}}, player, [PlayerId], []);
        Any ->
            {error, Any}
    end.

init([Id]) 
  when is_integer(Id) ->
    process_flag(trap_exit, true),
    ok = create_runtime(Id, self()),
    
    {ok, #module_data{ player_id = Id, 
                       explored_map = [],
                       discovered_tiles = [], 						   				  
                       self = self() }}.

stop(ProcessId) 
  when is_pid(ProcessId) ->
    io:fwrite("player - stop player process: ~w~n", [ProcessId]),
    gen_server:cast(ProcessId, stop).

stop(ProcessId, Reason) 
  when is_pid(ProcessId) ->
    gen_server:cast(ProcessId, {stop, Reason}).

terminate(_Reason, Data) ->
    ok = db:delete(connection, Data#module_data.player_id).

handle_cast('DISCONNECT', Data) ->
    {noreply, Data};

handle_cast({'SOCKET', Socket}, Data) ->
    Data1 = Data#module_data{ socket = Socket },
    {noreply, Data1};

handle_cast('UPDATE_PERCEPTION', Data) ->		
    log4erl:info("Player update perception"),
    {noreply, Data#module_data { update_perception = true} };

handle_cast({'ADD_PERCEPTION', _NewPerceptionData}, Data) ->	
    %Perception = Data#module_data.perception,
    %NewPerception = lists:append(NewPerceptionData, Perception),
    %UniquePerception = util:unique_list(NewPerception),
    %NewData = Data#module_data {perception = UniquePerception},
    
    {noreply, Data};    

handle_cast({'SEND_PERCEPTION'}, Data) ->    
            
    ObjectPerception = build_perception(Data#module_data.player_id),			            
    io:fwrite("discovered tiles: ~w~n", [Data#module_data.discovered_tiles]),			
    R = #perception {entities = ObjectPerception,
                     tiles = Data#module_data.discovered_tiles},
    io:fwrite("perception record: ~w~n",[R]),
    % Reset update perception state
    NewData = Data#module_data {update_perception = false },
    forward_to_client(R, NewData),            
    io:fwrite("Perception Modified.~n"),
    
    {noreply, NewData}; 

handle_cast({'SEND_BATTLE_INFO', BattleId, Armies}, Data) ->	
    
    R = #battle_info {battle_id = BattleId,
                      armies = Armies},
    
    forward_to_client(R, Data),
    {noreply, Data}; 

handle_cast({'SEND_BATTLE_ADD_ARMY', BattleId, ArmyInfo}, Data) ->
    
    R = #battle_add_army {battle_id = BattleId,
                          army = ArmyInfo},
    
    forward_to_client(R, Data),
    {noreply, Data};

handle_cast({'SEND_BATTLE_DAMAGE', BattleId, SourceId, TargetId, Damage}, Data) ->
    
    R = #battle_damage {battle_id = BattleId,
                        source_id = SourceId,
                        target_id = TargetId,
                        damage = Damage},
    
    forward_to_client(R, Data),
    {noreply, Data};

handle_cast({'SET_DISCOVERED_TILES', _, EntityX, EntityY}, Data) ->
    TileIndexList = map:get_surrounding_tiles(EntityX, EntityY),
    io:fwrite("player - DISCOVERED_TILES  TileIndexList: ~w~n", [TileIndexList]),
    TileList = map:get_explored_map(TileIndexList),
    ExploredMap = Data#module_data.explored_map,
    NewExploredMap = ExploredMap ++ TileList,
    
    NewData = Data#module_data { explored_map = NewExploredMap, 
                                 discovered_tiles = TileList},
    
    {noreply, NewData};

handle_cast(_ = #move{ id = ArmyId, x = X, y = Y}, Data) ->
    [Kingdom] = db:index_read(kingdom, Data#module_data.player_id, #kingdom.player_id), 
    
    case lists:member(ArmyId, Kingdom#kingdom.armies) of
        true -> 
            gen_server:cast(global:whereis_name({army, ArmyId}), {'SET_STATE_MOVE', X, Y});           
        false ->
            none
    end,
    
    {noreply, Data};

handle_cast(_ = #attack{ id = ArmyId, target_id = TargetId}, Data) ->
    [Kingdom] = db:index_read(kingdom, Data#module_data.player_id, #kingdom.player_id), 
    
    case db:read(army, TargetId) of
        [TargetArmy] ->    
            Guard = (lists:member(ArmyId, Kingdom#kingdom.armies)) and
                    (Data#module_data.player_id =/= TargetArmy#army.player_id),
            if
                Guard ->		 
                    gen_server:cast(global:whereis_name({army, ArmyId}), {'SET_STATE_ATTACK', TargetId});
                true ->
                    ok		
            end;
        _ ->
            ok
    end,
    
    {noreply, Data};

handle_cast(_ = #add_waypoint{id = ArmyId, x = X, y = Y}, Data) ->
    [Kingdom] = db:index_read(kingdom, Data#module_data.player_id, #kingdom.player_id), 
    
    case lists:member(ArmyId, Kingdom#kingdom.armies) of
        true -> 
            gen_server:cast(global:whereis_name({army, ArmyId}), {'ADD_WAYPOINT', X, Y});           
        false ->
            none
    end,

    {noreply, Data};

handle_cast(_ = #request_info{ type = Type, id = Id}, Data) ->
    
    case Type of 
        ?OBJECT_ARMY ->
            {ArmyId, PlayerId, UnitsInfo} = gen_server:call(global:whereis_name({army, Id}), {'GET_INFO'}),			
            
            if
                Data#module_data.player_id =:= PlayerId ->
                    R = #info_army {id = ArmyId,
                                    units = UnitsInfo},
                    forward_to_client(R, Data);
                true ->
                    PlayerId
            end;
        ?OBJECT_CITY ->
            case gen_server:call(global:whereis_name({city, Id}), {'GET_INFO', Data#module_data.player_id}) of
                {detailed, BuildingInfo, BuildingsQueueInfo, UnitsInfo, UnitsQueueInfo} ->
                    io:fwrite("BuildingsQueueInfo: ~w~n", [BuildingsQueueInfo]),
                    R = #info_city { id = Id, 
                                     buildings = BuildingInfo,
                                     buildings_queue = BuildingsQueueInfo, 
                                     units = UnitsInfo,
                                     units_queue = UnitsQueueInfo},
                    forward_to_client(R, Data);
                {generic, CityInfo} ->
                    CityInfo;
                {none} ->
                    ok
            end;
        ?OBJECT_BATTLE ->
            case gen_server:call(global:whereis_name({battle, Id}), {'GET_INFO', Data#module_data.player_id}) of
                {detailed, BattleId, Armies} ->
                    R = #battle_info { battle_id = BattleId,
                                       armies = Armies},
                    forward_to_client(R, Data);
                {generic, BattleId} ->
                    BattleId
            end;
        ?OBJECT_TRANSPORT ->
            case gen_server:call(global:whereis_name(transport_pid), {'GET_INFO', Id}) of
                {detailed, TransportId, UnitsInfo} ->
                    R = #transport_info { transport_id = TransportId,
                                          units = UnitsInfo},
                    forward_to_client(R, Data);
                {generic, TransportId} ->
                    TransportId
            end
    end, 
    
    {noreply, Data};

handle_cast(_ = #city_queue_unit{id = Id, unit_type = UnitType, unit_size = UnitSize, caste = Caste}, Data) ->
    case city:queue_unit(Id, Data#module_data.player_id, UnitType, UnitSize, Caste) of
        {city, queued_unit} ->
            RequestInfo = #request_info{ type = ?OBJECT_CITY, id = Id},
            gen_server:cast(self(), RequestInfo);
        {city, Error} ->            
            log4erl:info("Queue Unit - Error: ~w~n", [Error])        
    end,                        
    {noreply, Data};

handle_cast(_ = #city_queue_building{id = Id, building_type = BuildingType}, Data) ->
    case city:queue_building(Id, Data#module_data.player_id, BuildingType) of
        {city, queued_building} ->
            RequestInfo = #request_info{ type = ?OBJECT_CITY, id = Id},
            gen_server:cast(self(), RequestInfo);
        {city, error} ->
            ok
    end,
    {noreply, Data};

handle_cast(_ = #transfer_unit{unit_id = UnitId, source_id = SourceId, source_type = SourceType, target_id = TargetId, target_type = TargetType}, Data) ->
    
    SourceAtom = object:get_atom(SourceType),
    TargetAtom = object:get_atom(TargetType),
    SourcePid = object:get_pid(SourceAtom, SourceId),
    
    case gen_server:call(SourcePid, {'TRANSFER_UNIT', SourceId, UnitId, TargetId, TargetAtom}) of
        {transfer_unit, success} ->
            RequestSourceInfo = #request_info{ type = SourceType, id = SourceId},
            gen_server:cast(self(), RequestSourceInfo),            
            
            RequestTargetInfo = #request_info{ type = TargetType, id = TargetId},
            gen_server:cast(self(), RequestTargetInfo);           
        {transfer_unit, TError} ->
            log4erl:info("~w: Transfer Unit Error - ~w~n", [?MODULE, TError]);
        {receive_unit, RError} ->
            log4erl:info("~w: Receive Unit Error - ~w~n", [?MODULE, RError])
    end,   
    
    {noreply, Data};

handle_cast(_ = #battle_target{battle_id = BattleId, 
                               source_army_id = SourceArmyId,
                               source_unit_id = SourceUnitId, 
                               target_army_id = TargetArmyId,
                               target_unit_id = TargetUnitId}, Data) ->
    
    case gen_server:call(global:whereis_name({battle, BattleId}), {'ADD_TARGET', SourceArmyId, SourceUnitId, TargetArmyId, TargetUnitId}) of
        {battle_target, success} ->
            ok;
        {battle_target, error} ->
            %TODO Add error message
            ok
    end,
    
    {noreply, Data};

handle_cast(_ = #build_improvement{city_id = CityId,
                                   x = X,
                                   y = Y,
                                   improvement_type = ImprovementType }, Data) ->
    
    [Kingdom] = db:index_read(kingdom, Data#module_data.player_id, #kingdom.player_id),

    case lists:member(CityId, Kingdom#kingdom.cities) of
        true ->
            city:add_improvement(CityId, X, Y, ImprovementType);
        false ->
            log4erl:info("Add Improvement - City id does not exist for this player.")        
    end,
    
    {noreply, Data};

handle_cast(_ = #add_claim{ city_id = CityId,
                            x = X,
                            y = Y}, Data) ->

    [Kingdom] = db:index_read(kingdom, Data#module_data.player_id, #kingdom.player_id),
   
    case lists:member(CityId, Kingdom#kingdom.cities) of
        true ->
            city:add_claim(CityId, X, Y);
        false ->
            log4erl:info("Add Claim - City id does not exist for this player.")        
    end,
    
    {noreply, Data};

handle_cast(_ = #assign_task{ city_id = CityId,
                              population_id = PopulationId,
                              amount = Amount,
                              task_id = TaskId,
                              task_type = TaskType}, Data) ->

    [Kingdom] = db:index_read(kingdom, Data#module_data.player_id, #kingdom.player_id),
   
    case lists:member(CityId, Kingdom#kingdom.cities) of
        true ->
            city:assign_task(CityId, PopulationId, Amount, TaskId, TaskType);
        false ->
            log4erl:info("Assign task - City id does not exist for this player.")        
    end,

    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data};

handle_cast({stop, Reason}, Data) ->
    {stop, Reason, Data}.

handle_call('LOGOUT', _From, Data) ->
    Self = self(),
    io:fwrite("player - logout.~n"),
    
    %Save explored map to disk
    save_explored_map(Data#module_data.player_id, Data#module_data.explored_map),
    
    %Delete from game
    GamePID = global:whereis_name(game_pid),
    io:fwrite("player - LOGOUT  GamePID: ~w~n", [GamePID]),
    gen_server:cast(GamePID, {'DELETE_PLAYER', Data#module_data.player_id, Data#module_data.self}),
    
    %Stop character and player servers
    spawn(fun() -> timer:sleep(100), io:fwrite("spawn - stop player process: ~w~n", [Self]), player:stop(Self) end),   
    
    {reply, ok, Data};

handle_call('ID', _From, Data) ->
    {reply, Data#module_data.player_id, Data};

handle_call('SOCKET', _From, Data) ->
    {reply, Data#module_data.socket, Data};

handle_call(Event, From, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
                              {line, ?LINE},
                              {self, self()}, 
                              {message, Event}, 
                              {from, From}
                             ]),
    {noreply, Data}.

handle_info({'EXIT', _Pid, _Reason}, Data) ->
    %% child exit?
    {noreply, Data};

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

create_runtime(ID, ProcessId) 
  when is_number(ID),
       is_pid(ProcessId) ->
    PlayerConn = #connection {
                              player_id = ID,
                              process = ProcessId
                             },
    ok = db:write(PlayerConn).

forward_to_client(Event, Data) ->    
    if 
        Data#module_data.socket /= none ->
            Data#module_data.socket ! {packet, Event};
        true ->
            ok
    end.

build_perception(PlayerId) ->
    [Kingdom] = db:dirty_index_read(kingdom, PlayerId, #kingdom.player_id),
    
    Armies = Kingdom#kingdom.armies,
    Cities = Kingdom#kingdom.cities,
    
    ArmyVisibleList = entity_visible_list(Armies, army),
    CityVisibleList = entity_visible_list(Cities, city),
    
    io:fwrite("ArmyVisibleList: ~w~n", [ArmyVisibleList]),
    io:fwrite("CityVisibleList: ~w~n", [CityVisibleList]),
    
    EntireVisibleList = ArmyVisibleList ++ CityVisibleList,
    UniqueVisibleList = lists:usort(EntireVisibleList),
    
    io:fwrite("UniqueVisibleList: ~w~n", [UniqueVisibleList]),
    
    F = fun({ObjectId, ObjectPid}, PerceptionList) ->
                State = gen_server:call(ObjectPid, {'GET_STATE', ObjectId}),
                
                [{State#state.id, 
                  State#state.player_id, 				  
                  State#state.type,
                  State#state.state,
                  State#state.x,
                  State#state.y} | PerceptionList]
        end,
    
    lists:foldl(F, [], UniqueVisibleList).

entity_visible_list(EntityList, EntityType) ->
    
    F = fun(EntityId, EveryVisibleList) ->
                EntityPid = global:whereis_name({EntityType, EntityId}),
                VisibleList = gen_server:call(EntityPid, 'GET_VISIBLE'),
                VisibleListWithSelf = [{EntityId, EntityPid} | VisibleList],
                VisibleListWithSelf ++ EveryVisibleList 
        end,
    
    lists:foldl(F, [], EntityList).

get_explored_map(PlayerId) ->
    log4erl:info("Retrieving explored map."),
    FileName = "map" ++ integer_to_list(PlayerId) ++ ".dets",
    case dets:open_file(FileName,[{type, set}]) of
        {ok, DetsFile} ->
            ExploredTileIndex = dets:foldl(fun({X}, List) -> [X | List] end, [], DetsFile),
            dets:close(FileName),
            io:fwrite("player - get_explored_map  ExploredTileIndex: ~w~n", [ExploredTileIndex]),	
            ExploredMap = map:get_explored_map(ExploredTileIndex);
        {error, Reason} ->
            ExploredMap = [],
            log4erl:error("Could not open explored map files - Reason: ~p", [Reason])
    end,
    ExploredMap.

save_explored_map(PlayerId, ExploredMap) ->
    FileName = "map" ++ integer_to_list(PlayerId) ++ ".dets",
    {_,DetsFile} = dets:open_file(FileName,[{type, set}]),
    
    F = fun(Element) ->
                {TileIndex, _} = Element,   
                dets:insert(DetsFile, {TileIndex}) 
        end,
    
    lists:foreach(F, ExploredMap),
    dets:close(FileName).

