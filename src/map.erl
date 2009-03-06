%% Author: Peter
%% Created: Jan 26, 2009
%% Description: TODO: Add description to map
-module(map).
-behaviour(gen_server).
%%
%% Include files
%%
-include("common.hrl").
-include("schema.hrl").

%%
%% Exported Functions
%%
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-export([get_surrounding_tiles/2, start/0, stop/1]).

-record(module_data, {
          map,   
          self
         }).

%%
%% API Functions
%%

get_surrounding_tiles(X, Y) ->
    io:fwrite("player - get_surrounding_tiles x: ~w y: ~w~n",[X,Y]),
    CenterIndex = Y * ?MAP_HEIGHT + X,
       
    T0 = CenterIndex - ?MAP_WIDTH - 1,
    T1 = CenterIndex - ?MAP_WIDTH,
    T2 = CenterIndex - ?MAP_WIDTH + 1,
    T3 = CenterIndex - 1,
    T4 = CenterIndex,
    T5 = CenterIndex + 1,
    T6 = CenterIndex + ?MAP_WIDTH - 1,
    T7 = CenterIndex + ?MAP_WIDTH,
    T8 = CenterIndex + ?MAP_WIDTH + 1,
    
    TileList = [T0, T1, T2, T3, T4, T5, T6, T7, T8],
	TileList.

start() ->
	gen_server:start({global, map_pid}, map, [], []).

init([]) ->
  
    EmptyMapData = array:new([{size, ?MAP_NUMTILES}, {default, 0}, {fixed, true}]),
	{ok, S} = file:open(?MAP_FILENAME, read),
    FilledMapData = populate(0 , ?MAP_NUMTILES, EmptyMapData, S),  
    {ok, #module_data{ map = FilledMapData, self = self() }}.

terminate(_Reason, Data) ->
    ok.

stop(ProcessId) 
  when is_pid(ProcessId) ->
    gen_server:cast(ProcessId, stop).

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({'GET_MAP_BLOCK', CoordX, CoordY}, _From, Data) ->
    MapData = Data#module_data.map,
    
    %io:fwrite("map - get_tiles -> MapData: ~w~n", [MapData]),
    
    CornerX = (CoordX div ?MAP_BLOCK_WIDTH) * ?MAP_BLOCK_WIDTH,
    CornerY = (CoordY div ?MAP_BLOCK_HEIGHT) * ?MAP_BLOCK_HEIGHT,    
    
    MaxX = CornerX + ?MAP_BLOCK_WIDTH,
    MaxY = CornerY + ?MAP_BLOCK_HEIGHT,
    
    io:fwrite("map - CornerX: ~w CornerY: ~w~n", [CornerX, CornerY]),
    
    TileList = tiles_y(MaxY, MaxX, CornerY, CornerX, CornerY, CornerX, [], MapData),
    
    io:fwrite("map - get_tiles -> TileList: ~w~n", [TileList]),
        
	{reply, {CornerX, CornerY, TileList}, Data};

handle_call({'GET_EXPLORED_MAP', TileIndexList}, _From, Data) ->
    MapData = Data#module_data.map,   
  	MapTiles = get_map_tiles(TileIndexList, [], MapData),
     
    {reply, MapTiles, Data};

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

populate(Max, Max, Data, _) ->
     Data;

populate(N, Max, Data, S) ->
     [Head | _] = io:get_line(S, ''),
     NewData = array:set(N, Head, Data),
     %io:fwrite("populate - head: ~w newdata: ~w~n", [Head, NewData]),
     populate(N + 1, Max, NewData, S).
      
tiles_y(MaxY, _, _, CornerX, MaxY, CornerX, TileList, _) ->
    TileList;

tiles_y(MaxY, MaxX, CornerY, CornerX, IndexY, IndexX, TileList, MapData) ->
    
    %io:fwrite("tiles_y: ~w ~w MinX:~w~n", [IndexY, IndexX, MaxX]),
    
    Index = (IndexY * ?MAP_HEIGHT) + IndexX,
    NewMaxX = (IndexY * ?MAP_HEIGHT) + MaxX,
    
    NewTileList = tiles_x(NewMaxX, Index, TileList, MapData),    
    
    tiles_y(MaxY, MaxX, CornerY, CornerX, IndexY + 1, CornerX, NewTileList, MapData).


tiles_x(MaxX, MaxX, TileList, _) ->
    TileList;

tiles_x(MaxX, Index, TileList, MapData) ->
    
    Tile = array:get(Index, MapData),   
    %io:fwrite("tiles_x: ~w ~w ~w~n", [Tile, MaxX, Index]),
    
    tiles_x(MaxX, Index + 1, [Tile | TileList], MapData).

get_map_tiles([], MapList, _) ->
    MapList;

get_map_tiles(TileIndexList, MapList, MapData) ->
    [TileIndex | Rest] = TileIndexList,   
    
    if
        TileIndex > 0 ->
        	Tile = array:get(TileIndex, MapData),    
			NewMapList = [{TileIndex, Tile} | MapList];
        true ->
    		NewMapList = MapList
    end,

	get_map_tiles(Rest, NewMapList , MapData).
    

test_tiles_y(IndexX, _, _, _) ->
    ok;

test_tiles_y(IndexX, IndexY, BlockSize, TileList) ->
    
   NewTileList = test_tiles_x(IndexX, BlockSize, TileList),
   
   test_tiles_y(IndexX, IndexY + 1, BlockSize, NewTileList).

test_tiles_x(BlockSize, BlockSize, TileList) ->
    TileList;

test_tiles_x(Index, BlockSize, TileList) ->
    
    if
        Index > 0,
        Index < ?MAP_NUMTILES ->
     		NewTileList = [Index | TileList];
		true ->
			NewTileList = TileList
	end,
    
    test_tiles_x(Index + 1, BlockSize, NewTileList).  
