%% -------------------------------------------------------------------
%% Author  : Peter
%%% Description :
%%%
%%% Created : June 23, 2010
%%% -------------------------------------------------------------------
-module(map).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("schema.hrl").
-include("common.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([load/0, get_tile/2, get_tile_type/2, get_explored_map/1, get_surrounding_tiles/2]).
-export([get_tile_info/1, harvest_resource/3, is_tile_explored/2]).
-export([convert_coords/1, convert_coords/2]).
-record(module_data, {}).
%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, map}, map, [], []).

load() ->
    gen_server:call({global, map}, load).

get_tile(X, Y) ->
    TileIndex = convert_coords(X,Y),
    gen_server:call({global, map}, {'GET_TILE', TileIndex}).

get_tile_info(TileIndex) ->
    gen_server:call({global, map}, {'GET_TILE_INFO', TileIndex}).

get_tile_type(X, Y) ->
    TileIndex = convert_coords(X,Y),
    gen_server:call({global, map}, {'GET_TILE_TYPE', TileIndex}).

get_explored_map(TileIndexList) ->
    gen_server:call({global, map}, {'GET_EXPLORED_MAP', TileIndexList}).

get_surrounding_tiles(X, Y) ->
    Tiles2D = surrounding_tiles_2D(X, Y, 1),
    TileList = surrounding_tiles(Tiles2D),
    TileList.

harvest_resource(TileIndex, ResourceType, Amount) ->
    gen_server:call({global, map}, {'HARVEST_RESOURCE', TileIndex, ResourceType, Amount}).

is_tile_explored(TileIndex, ExploredMap) ->
    case lists:keyfind(TileIndex, 1, ExploredMap) of
        false ->
            Result = false;
        {_Tile, _TileType} ->
            Result = true
    end,
    Result.

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    Data = #module_data{},
    {ok, Data}.

handle_cast(none, Data) ->  
    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call(load, _From, Data) ->
    io:fwrite("Loading map...~n"),
    case file:open("tiles.bin", read) of
        {ok, TilesFileRef} ->
            load_tiles(TilesFileRef, false, 0),
            case file:open("resourceList.txt", read) of
                {ok, ResourceListFileRef} ->
                    Time = util:get_time_seconds(),
                    load_resources(ResourceListFileRef, Time, false, 0);
                Any ->
                    io:fwrite("Failed to open resourceList.txt - ~w", [Any])
            end;
        Any ->
            io:fwrite("Failed to open tiles.bin - ~w", [Any])
    end,   

    {reply, ok, Data};

handle_call({'GET_EXPLORED_MAP', TileIndexList}, _From, Data) ->
    MapTiles = get_map_tiles(TileIndexList, []),
    {reply, MapTiles, Data};

handle_call({'GET_TILE', TileIndex}, _From, Data) ->
    Tile = db:dirty_read(tile, TileIndex),
    {reply, Tile, Data};

handle_call({'GET_TILE_INFO', TileIndex}, _From, Data) ->
    case db:dirty_read(tile, TileIndex) of
        [Tile] ->
            TileType = Tile#tile.type,
            Resources = get_resources(Tile#tile.resources, []);
        _ ->
            TileType = -1,
            Resources = [],
            log4erl:error("~w: Could not find tile ~w", [?MODULE, TileIndex]),
            erlang:error("Could not find tile.")
    end,      
    {reply, {TileType, Resources}, Data};

handle_call({'GET_TILE_TYPE', TileIndex}, _From, Data) ->
    [Tile] = db:dirty_read(tile, TileIndex),
    {reply, Tile#tile.type, Data};

handle_call({'HARVEST_RESOURCE', TileIndex, ResourceType, Amount}, _From, Data) ->
    [Tile] = db:dirty_read(tile, TileIndex), 
    ?INFO("Resources: ", Tile#tile.resources),
    case lists:keyfind(ResourceType, #tile.resources, Tile#tile.resources) of
        false ->
            HarvestAmount = 0,
            log4erl:error("{~w}: Could not find resource type ~w", [?MODULE, ResourceType]),
            erlang:error("Could not find resource type.");
        {ResourceId, _ResType} ->
            HarvestAmount = get_harvest_amount(ResourceId, Amount)                
    end,                     
    {reply, HarvestAmount, Data};

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

terminate(_Reason, _) ->
    ok.

%% --------------------------------------------------------------------
%%% Internal functions

load_tiles(_FileRef, true, _TileIndex) ->
    ?INFO("loading tiles done"),
    done;

load_tiles(FileRef, _, TileIndex) ->
    case file:read(FileRef, 1) of
        {ok, Data} ->
            [TileType] = Data,
            Tile = #tile {index = TileIndex, type = TileType, resources = []},
            db:dirty_write(Tile),
            EOF = false;
        eof ->
            EOF = true
    end,
    load_tiles(FileRef, EOF, TileIndex + 1).
            
load_resources(_ResourceListFileRef, _Time, true, _TileIndex) ->
    ?INFO("Loading resources done"),
    done;

load_resources(ResourceListFileRef, Time, _, TileIndex) ->
    case io:get_line(ResourceListFileRef, '') of
        eof ->
            EOF = true;
        Data ->
            ResourceFileName = re:split(Data,"[\n]",[{return,list},trim]), 
            case file:open(ResourceFileName, read) of
                {ok, ResourceFileRef} ->
                    [FileName] = ResourceFileName,
                    [ResourceName, _Ext] = re:split(FileName, "[.]", [{return, list}, trim]),
                    case db:dirty_match_object({resource_type, '_', ResourceName}) of
                        [ResourceType] ->
                            load_resource_regen(ResourceFileRef, 
                                                ResourceType#resource_type.id, 
                                                Time, 
                                                false, 
                                                0);
                        _ ->
                            ?ERROR("Invalid resource type")
                    end;
                Any ->
                    io:fwrite("Failed to open ResourceFileName: ~w - ~w", [ResourceFileName, Any])
            end,
            EOF = false
    end,
    load_resources(ResourceListFileRef, Time, EOF, TileIndex + 1).

load_resource_regen(_ResourceFileRef, _ResourceType, _Time, true, _TileIndex) ->
    done;

load_resource_regen(ResourceFileRef, ResourceType, Time, _, TileIndex) ->
    case file:read(ResourceFileRef, 1) of
        {ok, Data} ->
            [ResourceRegen] = Data,
            case ResourceRegen > 0 of
                true ->
                    ResourceId = counter:increment(resource),
                    [Tile] = db:dirty_read(tile, TileIndex),

                    NewResources = [{ResourceId, ResourceType} | Tile#tile.resources],
                    NewTile = Tile#tile { resources = NewResources},       
                    Resource = #resource {id = ResourceId,
                                          type = ResourceType,
                                          total = 0,
                                          regen_rate = ResourceRegen,
                                          last_update = Time},

                    db:dirty_write(NewTile),
                    db:dirty_write(Resource);
                false ->
                    skip_resource
            end,                                            
            EOF = false;
        eof ->
            EOF = true
    end,
    load_resource_regen(ResourceFileRef, ResourceType, Time, EOF, TileIndex + 1).

get_harvest_amount(ResourceId, Amount) ->
    case db:dirty_read(resource, ResourceId) of
        [Resource] ->
            HarvestAmount = update_resource(Resource, Amount);
        _ ->
            HarvestAmount = 0
    end,
    HarvestAmount.

update_resource(Resource, Amount) ->
    CurrentTime = util:get_time_seconds(),
    DiffTime = CurrentTime - Resource#resource.last_update,
    
    Total = Resource#resource.total,
    ResourceGrowth = erlang:round(DiffTime / 10) * Resource#resource.regen_rate,

    case (Total + ResourceGrowth - Amount) < 0 of
        true ->
            HarvestAmount = Amount - (Total + ResourceGrowth),
            NewTotal = 0;
        false ->
            HarvestAmount = Amount,
            NewTotal = Total + ResourceGrowth - Amount
    end,
        
    NewResource = Resource#resource {total = NewTotal,
                                     last_update = CurrentTime},

    db:dirty_write(NewResource),
    %Return harvested amount
    HarvestAmount.

get_resources([], NewResources) ->
    NewResources;

get_resources([{ResourceId, _ResourceType} | Rest], Resources) ->
    case db:dirty_read(resource, ResourceId) of
        [Resource] ->
            NewResource = update_resource(Resource),
            NewResources = [NewResource | Resources];
        _ ->            
            NewResources = Resources,
            log4erl:error("~w: Could not find resource id ~w", [?MODULE, ResourceId]),
            erlang:error("Could not find resource id.")
    end,           
    
    get_resources(Rest, NewResources).

update_resource(Resource) ->
    CurrentTime = util:get_time_seconds(),
    DiffTime = CurrentTime - Resource#resource.last_update,
    log4erl:info("~w", [DiffTime]),
    log4erl:info("{~w} Resource Total ~w", [?MODULE, Resource#resource.total]),
    ResourceGrowth = erlang:round(DiffTime / 10) * Resource#resource.regen_rate,
    NewTotal = Resource#resource.total + ResourceGrowth,    

    NewResource = Resource#resource {total = NewTotal,
                                     last_update = CurrentTime},
    log4erl:info("{~w} New Resource Total ~w", [?MODULE, NewResource#resource.total]),
    db:dirty_write(NewResource),

    {NewResource#resource.id, 
     NewResource#resource.type,
     NewResource#resource.total,
     NewResource#resource.regen_rate}.

get_map_tiles([], MapList) ->
    MapList;

get_map_tiles(TileIndexList, MapList) ->
    [TileIndex | Rest] = TileIndexList,

    if
        TileIndex >= 0 ->
            [Tile] = db:dirty_read(tile, TileIndex),
            NewMapList = [{TileIndex, Tile#tile.type} | MapList];
        true ->
            NewMapList = MapList
    end,

    get_map_tiles(Rest, NewMapList).

convert_coords(X, Y) ->
    Y * ?MAP_HEIGHT + X.

convert_coords(TileIndex) ->
    TileX = TileIndex rem ?MAP_WIDTH,
    TileY = TileIndex div ?MAP_HEIGHT,
    {TileX , TileY}.

is_valid_coords(X, Y) ->
    GuardX = (X >= 0) and (X < ?MAP_WIDTH),
    GuardY = (Y >= 0) and (Y < ?MAP_HEIGHT),
    
    if
        (GuardX and GuardY) ->
            Result = true;
        true ->
            Result = false
    end,
    
    Result.

surrounding_tiles_2D(X, Y, ViewRange) ->
    MinX = X - ViewRange,
    MinY = Y - ViewRange,
    MaxX = X + ViewRange + 1,
    MaxY = Y + ViewRange + 1,
    tiles_y_2D(MinX, MinY, MaxX, MaxY, []).

tiles_y_2D(_, MaxY, _, MaxY, Tiles) ->
    Tiles;

tiles_y_2D(X, Y, MaxX, MaxY, Tiles) ->
    
    %io:fwrite("tiles_y_2D - x: ~w y: ~w MaxX: ~w MaxY: ~w Tiles: ~w~n", [X, Y, MaxX, MaxY, Tiles]),
    NewTiles = tiles_x_2D(X, Y, MaxX, MaxY, Tiles),
    tiles_y_2D(X, Y + 1, MaxX, MaxY, NewTiles).

tiles_x_2D(MaxX, _, MaxX, _, Tiles) ->
    Tiles;

tiles_x_2D(X, Y, MaxX, MaxY, Tiles) ->
    Tile = {X, Y},
    NewTiles = [Tile | Tiles],
    %io:fwrite("tiles_x_2D - x: ~w y: ~w MaxX: ~w MaxY: ~w NewTiles: ~w~n", [X, Y, MaxX, MaxY, NewTiles]),
    tiles_x_2D(X + 1, Y, MaxX, MaxY, NewTiles).

%% TODO: Combine with above tiles x,y looping
surrounding_tiles(Tiles2D) ->
    
    F = fun(Tile2D, Tiles) ->
                
                {X, Y} = Tile2D,
                ValidTile = is_valid_coords(X, Y),
                
                if
                    ValidTile ->
                        Tile = convert_coords(X, Y),
                        NewTiles = [Tile | Tiles];
                    true ->
                        NewTiles = Tiles
                end,
                
                NewTiles
        end,
    
    lists:foldl(F, [], Tiles2D).

     
