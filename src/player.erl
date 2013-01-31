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
-export([set_socket/2]).
-export([send_battle_event/4]).
-export([send_battle_info/4]).
-export([get_info_kingdom/1]).
%%
%% Records
%%

-record(data, {
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

send_battle_event(PlayerId, BattleEvent, BattleId, ArmyInfo) ->
    gen_server:cast(global:whereis_name({player, PlayerId}), {'SEND_BATTLE_EVENT', BattleEvent, BattleId, ArmyInfo}).

send_battle_info(PlayerId, BattleId, Armies, Items) ->
    gen_server:cast(global:whereis_name({player, PlayerId}), {'SEND_BATTLE_INFO', BattleId, Armies, Items}).

set_socket(Pid, Socket) ->
    gen_server:cast(Pid, {'SOCKET', Socket}).

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
    
    {ok, #data{ player_id = Id, 
                       explored_map = [],
                       discovered_tiles = [], 						   				  
                       self = self() }}.

stop(ProcessId) 
  when is_pid(ProcessId) ->
    log4erl:debug("player - stop player process: ~w~n", [ProcessId]),
    gen_server:cast(ProcessId, stop).

stop(ProcessId, Reason) 
  when is_pid(ProcessId) ->
    gen_server:cast(ProcessId, {stop, Reason}).

terminate(_Reason, Data) ->
    ok = db:delete(connection, Data#data.player_id).

handle_cast('DISCONNECT', Data) ->
    {noreply, Data};

handle_cast({'SOCKET', Socket}, Data) ->
    Data1 = Data#data{ socket = Socket },
    {noreply, Data1};

%handle_cast('UPDATE_PERCEPTION', Data) ->		
    %log4erl:debug("Player update perception"),
%    {noreply, Data#data { update_perception = true} };

handle_cast({'SEND_SETUP_INFO'}, Data) ->    
    ExploredMap = get_explored_map(Data#data.player_id),    
    Map = #map {tiles = ExploredMap},
    forward_to_client(Map, Data),
    send_perception(Data),

    get_info_kingdom(Data),
    get_info_all_cities(Data),

    NewData = Data#data {explored_map = ExploredMap},

    {noreply, NewData};

handle_cast({'SEND_PERCEPTION'}, Data) ->    
    send_perception(Data),    
    {noreply, Data}; 

handle_cast({'SEND_BATTLE_INFO', BattleId, Armies, Items}, Data) ->	
    R = #battle_info {battle_id = BattleId,
                      armies = Armies,
                      items = Items},
    
    forward_to_client(R, Data),
    {noreply, Data}; 

handle_cast({'SEND_BATTLE_EVENT', BattleEvent, BattleId, ArmyInfo}, Data) ->
    R = #battle_event {battle_event = BattleEvent, 
                       battle_id = BattleId,
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
    io:fwrite("Data: ~w~n", [Data]),
    TileIndexList = map:get_surrounding_tiles(EntityX, EntityY),
    TileList = map:get_explored_map(TileIndexList),
    ExploredMap = Data#data.explored_map,
    NewExploredMap = ExploredMap ++ TileList,
    
    NewData = Data#data { explored_map = NewExploredMap, 
                                 discovered_tiles = TileList},
    
    {noreply, NewData};

handle_cast(_ = #move{ id = ArmyId, x = X, y = Y}, Data) ->
    [Kingdom] = db:index_read(kingdom, Data#data.player_id, #kingdom.player_id), 
    
    case lists:member(ArmyId, Kingdom#kingdom.armies) of
        true ->
            army:move(ArmyId, X, Y);
        false ->
            log4erl:error("Army ~w is not owned by Player ~w", [ArmyId, #kingdom.player_id])
    end,
    
    {noreply, Data};

handle_cast(_ = #attack{ id = ArmyId, target_id = TargetId}, Data) ->
    log4erl:debug("player - attack~n"),
    [Kingdom] = db:index_read(kingdom, Data#data.player_id, #kingdom.player_id), 
    
    case db:read(army, TargetId) of
        [TargetArmy] ->    
            log4erl:debug("player - attack read army~n"),
            Guard = (lists:member(ArmyId, Kingdom#kingdom.armies)) and
                    (Data#data.player_id =/= TargetArmy#army.player_id),
            if
                Guard ->		
                    army:attack(ArmyId, TargetId);
                true ->
                    log4erl:error("Army ~w is not owned by Player ~w and
                                   Target ~w is the same as Player ~w", [ArmyId, 
                                                                         Data#data.player_id,
                                                                         TargetArmy#army.player_id,
                                                                         Data#data.player_id])                                                                         
            end;
        _ ->
            log4erl:error("Target ~w does not exist", [TargetId])
    end,
    
    {noreply, Data};

handle_cast(_ = #add_waypoint{id = ArmyId, x = X, y = Y}, Data) ->
    [Kingdom] = db:index_read(kingdom, Data#data.player_id, #kingdom.player_id), 
    
    case lists:member(ArmyId, Kingdom#kingdom.armies) of
        true -> 
            gen_server:cast(global:whereis_name({army, ArmyId}), {'ADD_WAYPOINT', X, Y});           
        false ->
            none
    end,

    {noreply, Data};

handle_cast(_ = #request_info{ type = Type, id = Id}, Data) ->
    %TODO VALIDATION
    case Type of
        ?OBJECT_TILE ->
            log4erl:debug("tile request info~n"),
            TileIndex = Id,
            case map:is_tile_explored(TileIndex, Data#data.explored_map) of
                true ->
                    log4erl:debug("tile is explored~n"),
                    {TileType, Resources} = map:get_tile_info(TileIndex),
                    ClaimTuple = claim:info(TileIndex),
                    R = #info_tile { tile_index = TileIndex,
                                     tile_type = TileType,
                                     resources = Resources,
                                     claim = ClaimTuple},
                    forward_to_client(R, Data);
                false ->
                    log4erl:debug("tile is not explored~n"),
                    ok
            end; 
        ?OBJECT_ARMY ->
            case global:whereis_name({army, Id}) of
                undefined -> ?ERROR("ArmyPid does not exist for Army Id: ");
                ArmyPid ->
                    case gen_server:call(ArmyPid, {'GET_INFO', Data#data.player_id}) of
                        {detailed, ArmyId, ArmyName, UnitsInfo} ->            
                            R = #info_army { id = ArmyId,
                                             name = ArmyName,
                                             units = UnitsInfo},
                            forward_to_client(R, Data);
                        {generic, ArmyId, ArmyPlayerId, ArmyName, KingdomName} ->
                            R = #info_generic_army { id = ArmyId,
                                                     player_id = ArmyPlayerId,
                                                     name = ArmyName,
                                                     kingdom_name = KingdomName},
                            forward_to_client(R, Data);
                        _ ->
                            log4erl:debug("Unmatched message from Army GET_INFO"),
                            ok
                    end
            end;
       ?OBJECT_CITY ->
            get_info_city(Id, Data);
       ?OBJECT_BATTLE ->
            case gen_server:call(global:whereis_name({battle, Id}), {'GET_INFO', Data#data.player_id}) of
                {detailed, BattleId, Armies, Items} ->
                    R = #battle_info { battle_id = BattleId,
                                       armies = Armies,
                                       items = Items},
                    forward_to_client(R, Data);
                {generic, BattleId} ->
                    BattleId
            end;
        ?OBJECT_IMPROVEMENT ->
            get_info_improvement(Id, Data);
        ?OBJECT_ITEM_RECIPE ->
            case item:get_recipe(Id, Data#data.player_id) of
                {true, ItemRecipe} ->
                    R = #info_item_recipe {type_id = ItemRecipe#item_recipe.type_id,
                                           template_id = ItemRecipe#item_recipe.template_id,
                                           player_id = ItemRecipe#item_recipe.player_id,
                                           item_name = ItemRecipe#item_recipe.item_name,
                                           flavour_text = ItemRecipe#item_recipe.flavour_text,
                                           material_amount = ItemRecipe#item_recipe.material_amount,
                                           material_type = ItemRecipe#item_recipe.material_type},
                    forward_to_client(R, Data);
                false ->
                    ok
            end;
        _ -> 
            log4erl:error("Invalid object type")
    end, 
    
    {noreply, Data};

handle_cast(_ = #city_form_army{city_id = CityId,
                                army_name = ArmyName}, Data) ->
    case kingdom:is_player_city(Data#data.player_id, CityId) of
        true ->
            case city:form_army(CityId, ArmyName) of
                {city, formed_army} ->
                    ?INFO("Form Army - Success"),
                    game:update_perception(Data#data.player_id);
                {city, Error} ->
                    ?INFO("Form Army - Error: ", Error)
            end;
        false ->
            ?INFO("CityId is not member of Kingdom")
    end,                        
    {noreply, Data};

handle_cast(_ = #city_queue_unit{city_id = CityId, 
                                 building_id = BuildingId,
                                 unit_type = UnitType, 
                                 unit_size = UnitSize,
                                 caste = Caste,
                                 race = Race}, Data) ->

    case kingdom:is_player_city(Data#data.player_id, CityId) of
        true ->
            case city:queue_unit(CityId, BuildingId, UnitType, UnitSize, Caste, Race) of
                {city, queued_unit} ->
                    RequestInfo = #request_info{ type = ?OBJECT_CITY, id = CityId},
                    gen_server:cast(self(), RequestInfo);
                {city, Error} ->            
                    ?INFO("Queue Unit - Error: ",Error)
            end;
        false ->
            ?INFO("CityId is not member of Kingdom")
    end,                        
    {noreply, Data};

handle_cast(_ = #city_queue_building{city_id = CityId, building_type = BuildingType}, Data) ->
    
    case kingdom:is_player_city(Data#data.player_id, CityId) of
        true ->
            case city:queue_building(CityId, BuildingType) of
                {city, queued_building} ->
                    RequestInfo = #request_info{ type = ?OBJECT_CITY, id = CityId},
                    gen_server:cast(self(), RequestInfo);
                {city, Error} ->
                    ?INFO("Queue Building - Error: ", Error)
            end;
        false ->
            ?INFO("CityId is not member of Kingdom")
    end,
    {noreply, Data};

handle_cast(_ = #city_queue_improvement{city_id = CityId,
                                        x = X,
                                        y = Y,
                                        improvement_type = ImprovementType }, Data) ->
    
    [Kingdom] = db:index_read(kingdom, Data#data.player_id, #kingdom.player_id),

    case lists:member(CityId, Kingdom#kingdom.cities) of
        true ->
            case city:queue_improvement(CityId, X, Y, ImprovementType) of
                {city, queued_improvement} ->
                    R = #success { type = ?CMD_CITY_QUEUE_IMPROVEMENT,
                                   id = -1},
                    forward_to_client(R, Data);               
                {city, Error} ->
                    log4erl:error("Queue Improvement - Error: ~w", [Error])
            end;
        false ->
            log4erl:info("Queue Improvement - City id does not exist for this player.")        
    end,
    
    {noreply, Data};

handle_cast(_ = #city_craft_item{city_id = CityId, 
                                 source_id = SourceId,
                                 source_type = SourceType,
                                 item_type = ItemType, 
                                 amount = Amount}, Data) ->

    case city:craft_item(CityId, SourceId, SourceType, ItemType, Amount) of
        {city, queued_crafted} ->
            RequestInfo = #request_info{ type = ?OBJECT_CITY, id = CityId},
            gen_server:cast(self(), RequestInfo);
        {city, queued_item} ->
            RequestInfo = #request_info{ type = ?OBJECT_CITY, id = CityId},
            gen_server:cast(self(), RequestInfo);
        {city, Error} ->            
            log4erl:error("Queue Item - Error: ~w", [Error])
    end,
    {noreply, Data};

handle_cast(_ = #add_item_recipe{template_id = TemplateId,
                                 player_id = PlayerId,
                                 item_name = ItemName,
                                 flavour_text = FlavourText,
                                 material_type = MaterialType}, Data) ->

    case Data#data.player_id =:= PlayerId of
        true ->
            case item:add_recipe(TemplateId, PlayerId, ItemName, FlavourText, MaterialType) of
                {success, RecipeId} ->
                    R = #success { type = ?CMD_ADD_ITEM_RECIPE,
                                   id = RecipeId},
                    forward_to_client(R, Data);
                {error, ErrorMsg} ->
                    ?ERROR(ErrorMsg)
            end;
        false ->
            ?ERROR("Invalid player_id: ", PlayerId)
    end,
    {noreply, Data};

handle_cast(_ = #add_unit_recipe{template_id = TemplateId,
                                 player_id = PlayerId,
                                 unit_name = UnitName,
                                 default_size = DefaultSize,
                                 gear = Gear}, Data) ->

    case Data#data.player_id =:= PlayerId of
        true ->
            case unit:add_recipe(TemplateId, PlayerId, UnitName, DefaultSize, Gear) of
                {success, RecipeId} ->
                    R = #success { type = ?CMD_ADD_UNIT_RECIPE,
                                   id = RecipeId},
                    forward_to_client(R, Data);
                {error, ErrorMsg} ->
                    ?ERROR(ErrorMsg)
            end;
        false ->
            ?ERROR("Invalid player_id: ", PlayerId)
    end,
    {noreply, Data};

handle_cast(_ = #transfer_item{item_id = ItemId, source_id = SourceId, source_type= SourceType, target_id = TargetId, target_type = TargetType}, Data) ->
    log4erl:info("Transfer Item - ItemId: ~w SourceId: ~w SourceType: ~w TargetId: ~w TargetType: ~w", [ItemId,
                                                                                                        SourceId,
                                                                                                        SourceType,
                                                                                                        TargetId,
                                                                                                        TargetType]),
    %TODO Add validation 
    item:transfer(ItemId, {TargetType, TargetId}, Data#data.player_id),
    R = #success { type = ?CMD_TRANSFER_ITEM,
                   id = -1},
    forward_to_client(R, Data),    
 
    {noreply, Data};

handle_cast(_ = #transfer_unit{unit_id = UnitId, source_id = SourceId, source_type = SourceType, target_id = TargetId, target_type = TargetType}, Data) ->   
    %TODO ADD validation
    ?INFO("Transfer Unit Id: ", UnitId, " EntityId: ", TargetId),
    
    unit:transfer(UnitId, TargetId),
    case SourceType of
        ?OBJECT_ARMY ->
            army:unit_transfered(SourceId);
        _ ->
            none
    end,

    case TargetType of
        ?OBJECT_ARMY ->
            army:unit_transfered(TargetId);
        _ ->
            none
    end,

    R = #success { type = ?CMD_TRANSFER_UNIT,
                   id = -1},
    forward_to_client(R, Data),

    {noreply, Data};

handle_cast(_ = #battle_target{battle_id = BattleId, 
                               source_army_id = SourceArmyId,
                               source_unit_id = SourceUnitId, 
                               target_army_id = TargetArmyId,
                               target_unit_id = TargetUnitId}, Data) ->
    case db:read(battle, BattleId) of
        [_Battle] ->
            case gen_server:call(global:whereis_name({battle, BattleId}), {'ADD_TARGET', SourceArmyId, SourceUnitId, TargetArmyId, TargetUnitId}) of
                {battle_target, success} ->
                    ok;
                {battle_target, Error} ->
                    log4erl:error("Battle Target Error - ~w", [Error])
            end;
        _ ->
            ok
    end,    
    
    {noreply, Data};

handle_cast(_ = #battle_retreat{battle_id = BattleId,
                                source_army_id = SourceArmyId}, Data) ->
    log4erl:info("~w: Battle Retreat command received - BattleId: ~w",[?MODULE, BattleId]),
    case db:read(battle, BattleId) of
        [_Battle] ->
            case gen_server:call(global:whereis_name({battle, BattleId}), {'RETREAT', SourceArmyId}) of
                {battle_retreat, success} ->
                    ok;
                {battle_retreat, error} ->
                    ok
            end;
        _ ->
            ok
    end,
    {noreply, Data};

handle_cast(_ = #battle_leave{battle_id = BattleId,
                              source_army_id = SourceArmyId}, Data) ->
    log4erl:info("~w: Battle Leave command received - BattleId: ~w",[?MODULE, BattleId]),
    case db:read(battle, BattleId) of
        [_Battle] ->
            case gen_server:call(global:whereis_name({battle, BattleId}), {'LEAVE', SourceArmyId}) of
                {battle_leave, success} ->
                    ok;
                {battle_leave, error} ->
                    ok
            end;
        _ ->
            ok
    end,
    {noreply, Data};

handle_cast(_ = #add_claim{ city_id = CityId,
                            army_id = ArmyId,
                            x = X,
                            y = Y}, Data) ->

    [Kingdom] = db:index_read(kingdom, Data#data.player_id, #kingdom.player_id),
   
    IsValid = (lists:member(CityId, Kingdom#kingdom.cities) and
               lists:member(ArmyId, Kingdom#kingdom.armies)),

    log4erl:info("Player - isValid: ~w", [IsValid]),
    log4erl:info("Player - ~w", [Kingdom#kingdom.armies]),

    case IsValid of
        true ->
            case city:add_claim(CityId, ArmyId, X, Y) of
                {success, ClaimId} ->
                    %Set Army State to Claim
                    army:claim(ArmyId, ClaimId),

                    R = #success { type = ?CMD_ADD_CLAIM,
                                   id = ClaimId},
                    forward_to_client(R, Data);
                {failure, Error} -> 
                    log4erl:error("Add Claim failed: ~w", [Error])
            end;
        false ->
            log4erl:info("Add Claim - City id does not exist for this player.")        
    end,
    
    {noreply, Data};

handle_cast(_ = #remove_claim{ city_id = CityId, claim_id = ClaimId}, Data) ->
    [Kingdom] = db:index_read(kingdom, Data#data.player_id, #kingdom.player_id),
    
    case lists:member(CityId, Kingdom#kingdom.cities) of
        true ->
            case city:remove_claim(CityId, ClaimId) of
                {success, Id} ->
                    R = #success { type = ?CMD_REMOVE_CLAIM,
                                   id = Id},
                    forward_to_client(R, Data);
                {failure, Error} ->
                    log4erl:error("{~w} Remove Claim error: ~w", [?MODULE, Error])
            end;
        false ->
            log4erl:info("{~w} Remove Claim - city id does not exist for this player.")
    end,
    
    {noreply, Data};

handle_cast(_ = #assign_task{ city_id = CityId,
                              caste  = Caste,
                              race = Race,
                              amount = Amount,
                              target_id = TaskId,
                              target_type = TaskType}, Data) ->

    [Kingdom] = db:index_read(kingdom, Data#data.player_id, #kingdom.player_id),
   
    case lists:member(CityId, Kingdom#kingdom.cities) of
        true ->
            case city:assign_task(CityId, Caste, Race, Amount, TaskId, TaskType) of
                {success, Id} ->
                    R = #success { type = ?CMD_ASSIGN_TASK,
                                   id = Id},
                    forward_to_client(R, Data);
                {failure, Error} ->
                    log4erl:error("Assign task failed: ~w", [Error])
            end;
        false ->
            log4erl:info("Assign task - City id does not exist for this player.")        
    end,

    {noreply, Data};

handle_cast(_ = #remove_task{ city_id = CityId,
                              assignment_id = AssignmentId}, Data) ->
    [Kingdom] = db:index_read(kingdom, Data#data.player_id, #kingdom.player_id),
   
    case lists:member(CityId, Kingdom#kingdom.cities) of
        true ->
            case city:remove_task(CityId, AssignmentId) of
                {success, Id} ->
                    R = #success { type = ?CMD_REMOVE_TASK,
                                   id = Id},
                    forward_to_client(R, Data);
                {failure, Error} ->
                    log4erl:error("{~w} Remove task failed: ~w", [?MODULE, Error])
            end;
        false ->
            log4erl:info("{~w} Remove task failed - city id does not exist for this player.")
    end,

    {noreply, Data}; 
    
handle_cast(_ = #create_sell_order{ item_id = ItemId,
                                    price = Price}, Data) ->
    log4erl:info("Create Sell Order item_id ~w price ~w", [ItemId, Price]),
    case db:dirty_read(item, ItemId) of
        [Item] ->
            {_EntityId, PlayerId} = Item#item.ref,
            case Data#data.player_id =:= PlayerId of
                true ->
                    market:create_sell_order(ItemId, Price);
                false ->
                    log4erl:error("Cannot create sell order for another player.")
            end;
        _ ->
            log4erl:error("Invalid item")
    end,

    {noreply, Data};

handle_cast(_ = #create_buy_order{  city_id = CityId,
                                    item_type = ItemTypeId,
                                    volume = Volume,
                                    price = Price}, Data) ->
    case db:dirty_read(item_type, ItemTypeId) of
        [_ItemType] ->
            market:create_buy_order(CityId, 
                                    Data#data.player_id, 
                                    ItemTypeId, 
                                    Volume,
                                    Price);
        _ ->
            log4erl:error("Invalid item type")
    end,

    {noreply, Data};

handle_cast(_ = #fill_sell_order{order_id = OrderId,
                                 volume = Volume}, Data) ->
    log4erl:info("Fill sell order id ~w volume ~w", [OrderId, Volume]),
    market:fill_sell_order(Data#data.player_id, OrderId, Volume),
    
    {noreply, Data};

handle_cast(_ = #fill_buy_order{order_id = OrderId,
                                volume = Volume}, Data) ->
    log4erl:info("Fill buy order id ~w volume ~w", [OrderId, Volume]),
    market:fill_buy_order(Data#data.player_id, OrderId, Volume),

    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data};

handle_cast({stop, Reason}, Data) ->
    {stop, Reason, Data}.

handle_call('LOGOUT', _From, Data) ->
    Self = self(),
    io:fwrite("player - logout.~n"),
    
    %Save explored map to disk
    save_explored_map(Data#data.player_id, Data#data.explored_map),
    
    %Delete from game
    GamePID = global:whereis_name(game_pid),
    io:fwrite("player - LOGOUT  GamePID: ~w~n", [GamePID]),
    gen_server:cast(GamePID, {'DELETE_PLAYER', Data#data.player_id, Data#data.self}),
    
    %Stop character and player servers
    spawn(fun() -> timer:sleep(100), io:fwrite("spawn - stop player process: ~w~n", [Self]), player:stop(Self) end),   
    
    {reply, ok, Data};

handle_call('ID', _From, Data) ->
    {reply, Data#data.player_id, Data};

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
        Data#data.socket /= none ->
            Data#data.socket ! {packet, Event};
        true ->
            ok
    end.

send_perception(Data) ->
    ObjectPerception = build_perception(Data#data.player_id),			            
    R = #perception {entities = ObjectPerception,
                     tiles = Data#data.discovered_tiles},
    % Reset update perception state
    forward_to_client(R, Data),            
    log4erl:debug("Perception Modified.~n").

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
                  State#state.subtype,
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

get_info_kingdom(Data) ->
    {Id, Name, Gold} = kingdom:get_info_kingdom(Data#data.player_id),
    ItemRecipes = item:get_recipes(Data#data.player_id),
    ?INFO("ItemRecipes: ", ItemRecipes),
    UnitRecipes = unit:get_recipes(Data#data.player_id),
    ?INFO("UnitRecipes: ", UnitRecipes),

    InfoKingdom = #info_kingdom {id = Id, 
                                 name = Name, 
                                 gold = Gold,
                                 item_recipes = ItemRecipes,
                                 unit_recipes = UnitRecipes},

    forward_to_client(InfoKingdom, Data).    

get_info_all_cities(Data) ->
    Cities = kingdom:get_cities(Data#data.player_id),
    
    F = fun(CityId) ->
            get_info_city(CityId, Data)
        end,
    lists:foreach(F, Cities).
    
get_info_city(Id, Data) ->
    case gen_server:call(global:whereis_name({city, Id}), {'GET_INFO', Data#data.player_id}) of
        {detailed, 
        CityName, 
        BuildingInfo, 
        UnitsInfo, 
        Claims,
        Improvements,
        Assignments,
        Items,
        Populations,
        Contracts} ->
            R = #info_city { id = Id,
                             name = CityName, 
                             buildings = BuildingInfo,
                             units = UnitsInfo,
                             claims = Claims,
                             improvements = Improvements,
                             assignments = Assignments,
                             items = Items,
                             populations = Populations,
                             contracts = Contracts},
            forward_to_client(R, Data);
        {generic, CityId, CityPlayerId, CityName, KingdomName} ->
            R = #info_generic_city { id = CityId,
                                     player_id = CityPlayerId,
                                     name = CityName,
                                     kingdom_name = KingdomName},
            forward_to_client(R, Data);                                             
        _ ->
            ok
    end.

get_info_improvement(Id, Data) ->
    case improvement:info(Id, Data#data.player_id) of
        {detailed, _Type} ->
            ok;
        _ ->    
            ok
    end.
