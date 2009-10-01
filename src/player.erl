%% Author: Peter
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

-export([start/1, stop/1, stop/2, get_explored_map/1]).

%%
%% Records
%%

-record(module_data, {
          player_id,
          socket = none, 
          last_perception = [],
          perception = [],
          explored_map = [], 
          discovered_tiles = [],  
		  discovered_improvements = [],		    
          self
         }).

%%
%% API Functions
%%

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

init([ID]) 
  when is_integer(ID) ->
    process_flag(trap_exit, true),
    ok = create_runtime(ID, self()),
    {ok, #module_data{ player_id = ID, 
                       perception = [], 
                       last_perception = [],
                       explored_map = [],
                       discovered_tiles = [], 
					   discovered_improvements = [],
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

handle_cast({'ADD_PERCEPTION', NewPerceptionData}, Data) ->
    Perception = Data#module_data.perception,
    NewPerception = lists:append(NewPerceptionData, Perception),
    UniquePerception = util:unique_list(NewPerception),
    NewData = Data#module_data {perception = UniquePerception},
    {noreply, NewData};    

handle_cast({'SEND_PERCEPTION'}, Data) ->
  	LastPerception = Data#module_data.last_perception,
    NewPerception = Data#module_data.perception,
    
    if 
        LastPerception =:= NewPerception ->
            NewData = Data#module_data{perception = []};       
		true ->
            NewData = Data#module_data {last_perception = NewPerception, perception = [] },
            R = #perception {entities = NewPerception,
                             tiles = Data#module_data.discovered_tiles,
							 improvements = Data#module_data.discovered_improvements
                            },
            forward_to_client(R, NewData),
            
            io:fwrite("Perception Modified. ~w~n", [NewPerception])   
   	end,
    
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
	NewDiscoveredImprovements = improvements:get_improvements(TileList),
	
    NewData = Data#module_data { explored_map = NewExploredMap, 
								 discovered_tiles = TileList,
								 discovered_improvements = NewDiscoveredImprovements},
      
    {noreply, NewData};

handle_cast(_ = #move{ id = ArmyId, x = X, y = Y}, Data) ->
    [Player] = db:read(player, Data#module_data.player_id), 
    
    case lists:member(ArmyId, Player#player.armies) of
		true -> 
            gen_server:cast(global:whereis_name({army, ArmyId}), {'SET_STATE_MOVE', X, Y});           
		false ->
            none
   	end,
    
    {noreply, Data};

handle_cast(_ = #attack{ id = ArmyId, target_id = TargetId}, Data) ->
    [Player] = db:read(player, Data#module_data.player_id),

    case db:read(army, TargetId) of
        [_] ->    
    		Guard = (lists:member(ArmyId, Player#player.armies)) and
            		(Player#player.id =/= TargetId),
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
                {detailed, BuildingInfo, UnitsInfo, UnitsQueueInfo} ->
                    R = #info_city { id = Id, 
                                     buildings = BuildingInfo, 
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
			end
    end, 
    
	{noreply, Data};

handle_cast(_ = #city_queue_unit{id = Id, unit_type = UnitType, unit_size = UnitSize}, Data) ->
	case city:queue_unit(Id, Data#module_data.player_id, UnitType, UnitSize) of
    %case gen_server:call(global:whereis_name({city, Id}), {'QUEUE_UNIT', Data#module_data.player_id, UnitType, UnitSize}) of
        {city, queued_unit} ->
            RequestInfo = #request_info{ type = ?OBJECT_CITY, id = Id},
            gen_server:cast(self(), RequestInfo);
        {city, error} ->
            ok
    end,                       
    
	{noreply, Data};

handle_cast(_ = #transfer_unit{unit_id = UnitId, source_id = SourceId, source_type = SourceType, target_id = TargetId, target_type = TargetType}, Data) ->
    
    SourceAtom = object:get_object_atom(SourceType),
    TargetAtom = object:get_object_atom(TargetType),
    
	case gen_server:call(global:whereis_name({SourceAtom, SourceId}), {'TRANSFER_UNIT', UnitId, TargetId, TargetAtom}) of
        {transfer_unit, success} ->
            RequestSourceInfo = #request_info{ type = SourceType, id = SourceId},
            gen_server:cast(self(), RequestSourceInfo),            
            
            RequestTargetInfo = #request_info{ type = TargetType, id = TargetId},
            gen_server:cast(self(), RequestTargetInfo);   
        	
        {transfer_unit, error} ->
            %TODO Add error message
            io:fwrite("player - transfer unit - failed~n");
        
        {receive_unit, error} ->
            io:fwrite("player - receive unit - failed~n")
            
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

handle_cast(_ = #build_improvement{improvement_type = ImprovementType, tile_index = TileIndex, source_army_id = ArmyId}, Data) ->

	
    
    
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

handle_call('GET_ARMIES', _From, Data) ->
    [Player] = db:read(player, Data#module_data.player_id), 
    {reply, Player#player.armies, Data};

handle_call('GET_ARMIES_PID', _From, Data) ->
    [Player] = db:read(player, Data#module_data.player_id),
    Armies = Player#player.armies,
        
    F = fun(ArmyId, Rest) -> [global:whereis_name({army, ArmyId}) | Rest] end,
    
    ArmiesPid = lists:foldl(F, [], Armies),
	io:fwrite("player - GET_ARMIES_PID  ArmiesPid: ~w~n", [ArmiesPid]),    
    {reply, ArmiesPid, Data};

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

get_explored_map(PlayerId) ->
  	FileName = "map" ++ integer_to_list(PlayerId) ++ ".dets",
	{_,DetsFile} = dets:open_file(FileName,[{type, set}]),
	ExploredTileIndex = dets:foldl(fun({X}, List) -> [X | List] end, [], DetsFile),
    dets:close(FileName),
    io:fwrite("player - get_explored_map  ExploredTileIndex: ~w~n", [ExploredTileIndex]),	
	ExploredMap = map:get_explored_map(ExploredTileIndex),
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
    