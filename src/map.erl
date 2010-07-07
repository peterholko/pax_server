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
-export([get_resource_yield/2]).
-export([convert_coords/1, convert_coords/2]).
-record(module_data, {map}).
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

get_tile_type(X, Y) ->
    TileIndex = convert_coords(X,Y),
    gen_server:call({global, map}, {'GET_TILE_TYPE', TileIndex}).

get_explored_map(TileIndexList) ->
    gen_server:call({global, map}, {'GET_EXPLORED_MAP', TileIndexList}).

get_surrounding_tiles(X, Y) ->
    io:fwrite("map - get_surrounding_tiles x: ~w y: ~w~n",[X,Y]),
    Tiles2D = surrounding_tiles_2D(X, Y, 1),
    io:fwrite("map - get_surrounding_tiles Tiles2D: ~w~n",[Tiles2D]),
    TileList = surrounding_tiles(Tiles2D),
    io:fwrite("map - get_surrounding_tiles TileList: ~w~n",[TileList]),
    TileList.

get_resource_yield(TileIndex, ResourceType) ->
    gen_server:call({global, map}, {'GET_RESOURCE_YIELD', TileIndex, ResourceType}).


%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    Data = #module_data {map = ets:new(map, [set])},
    {ok, Data}.

handle_cast(none, Data) ->  
    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call(load, _From, Data) ->
    io:fwrite("Loading map...~n"),
    case file:open("tiles.bin", read) of
        {ok, TilesFileRef} ->
            load_tiles(TilesFileRef, false, 0, Data#module_data.map),
            case file:open("resourceList.txt", read) of
                {ok, ResourceListFileRef} ->
                    load_resources(ResourceListFileRef, false, 0, Data#module_data.map);
                Any ->
                    io:fwrite("~w", [Any])
            end;
        Any ->
            io:fwrite("~w", [Any])
    end,   

    {reply, ok, Data};

handle_call({'GET_EXPLORED_MAP', TileIndexList}, _From, Data) ->
    MapTiles = get_map_tiles(TileIndexList, [], Data#module_data.map),
    {reply, MapTiles, Data};

handle_call({'GET_TILE', TileIndex}, _From, Data) ->
    MapTile = ets:lookup(Data#module_data.map, TileIndex),
    io:fwrite("Map Tile: ~w~n", [MapTile]),
    {reply, ok, Data};

handle_call({'GET_TILE_TYPE', TileIndex}, _From, Data) ->
    Tile = ets:lookup(Data#module_data.map, TileIndex),
    [{_Index, TileType, _Resources}] = Tile,
    {reply, TileType, Data};

handle_call({'GET_RESOURCE_YIELD', TileIndex, ResourceType}, _From, Data) ->
    Tile = ets:lookup(Data#module_data.map, TileIndex),
    [{_Index, _TileType, Resources}] = Tile,
    case lists:keyfind(ResourceType, 1, Resources) of
        false ->
            ResourceYield = 0,
            log4erl:error("~w: Could not find resource type ~w", [?MODULE, ResourceType]),
            erlang:error("Could not find resource type.");
        {ResourceType, ResourceYield} ->
            ok
    end,                     
    {reply, ResourceYield, Data};

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

load_tiles(_FileRef, true, _TileIndex, _Map) ->
    done;

load_tiles(FileRef, _, TileIndex, Map) ->
    case file:read(FileRef, 1) of
        {ok, Data} ->
            [TileType] = Data,
            ets:insert(Map, {TileIndex, TileType, []}),
            EOF = false;
        eof ->
            EOF = true
    end,
    load_tiles(FileRef, EOF, TileIndex + 1, Map).
            
load_resources(_ResourceListFileRef, true, _TileIndex, _Map) ->
    done;

load_resources(ResourceListFileRef, _, TileIndex, Map) ->
    case io:get_line(ResourceListFileRef, '') of
        eof ->
            EOF = true;
        Data ->
            ResourceFileName = Data, 
            case file:open(ResourceFileName, read) of
                {ok, ResourceFileRef} ->
                    load_resource_yield(ResourceFileRef, false, 0, Map);
                Any ->
                    io:fwrite("~w", [Any])
            end,
            EOF = false
    end,
    load_resources(ResourceListFileRef, EOF, TileIndex + 1, Map).

load_resource_yield(_ResourceFileRef, true, _TileIndex, _Map) ->
    done;

load_resource_yield(ResourceFileRef, _, TileIndex, Map) ->
    case file:read(ResourceFileRef, 1) of
        {ok, Data} ->
            %io:fwrite("Resource Data: ~w~n", [Data]),
            [ResourceYield] = Data,
            Tile = ets:lookup(Map, TileIndex),
            %io:fwrite("Tile: ~w~n", [Tile]),
            [{TileIndex, TileType, Resources}] = Tile,
            %io:fwrite("Resources: ~w~n",[Resources]),
            NewResources = [{1, ResourceYield} | Resources],
            ets:insert(Map, {TileIndex, TileType, NewResources}),
            EOF = false;
        eof ->
            EOF = true
    end,
    load_resource_yield(ResourceFileRef, EOF, TileIndex + 1, Map).

get_map_tiles([], MapList, _Map) ->
    MapList;

get_map_tiles(TileIndexList, MapList, Map) ->
    [TileIndex | Rest] = TileIndexList,

    if
        TileIndex >= 0 ->
            Tile = ets:lookup(Map, TileIndex),
            io:fwrite("Tile[~w]: ~w~n",[TileIndex, Tile]),
            [{_Index, TileType, _Resources}] = Tile,
            NewMapList = [{TileIndex, TileType} | MapList];
        true ->
            NewMapList = MapList
    end,

    get_map_tiles(Rest, NewMapList, Map).

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

     
