%% Author: Peter
%% Created: Jan 26, 2009
%% Description: TODO: Add description to map
-module(map).
-behaviour(gen_server).
%%
%% Include files
%%
-include("common.hrl").

%%
%% Exported Functions
%%
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-export([start/0, stop/1]).

-record(module_data, {
          map,   
          self
         }).

%%
%% API Functions
%%

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

handle_call({'GET_TILES', CenterX, CenterY}, _From, Data) ->
    MapData = Data#module_data.map,
    
    io:fwrite("map - get_tiles -> MapData: ~w~n", [MapData]),
    
    MinX = CenterX - 2,
    MinY = CenterY - 2,
    CornerX = CenterX + 1,
    CornerY = CenterX + 1,
    
    TileList = tiles_y(MinY, MinX, CornerY, CornerX, CornerY, CornerX, [], MapData),
    
    io:fwrite("map - get_tiles -> TileList: ~w~n", [TileList]),
    
	{reply, 0, Data};

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
     [Head | Rest] = io:get_line(S, ''),
     NewData = array:set(N, Head, Data),
     io:fwrite("populate - head: ~w newdata: ~w~n", [Head, NewData]),
     populate(N + 1, Max, NewData, S).
      
tiles_y(MinY, _, _, CornerX, MinY, CornerX, TileList, _) ->
    TileList;

tiles_y(MinY, MinX, CornerY, CornerX, IndexY, IndexX, TileList, MapData) ->
    
    io:fwrite("tiles_y: ~w ~w MinX:~w~n", [IndexY, IndexX, MinX]),
    
    Index = (IndexY * ?MAP_HEIGHT) + IndexX,
    NewMinX = (IndexY * ?MAP_HEIGHT) + MinX,
    
    NewTileList = tiles_x(NewMinX, Index, TileList, MapData),    
    
    tiles_y(MinY, MinX, CornerY, CornerX, IndexY - 1, CornerX, NewTileList, MapData).


tiles_x(MinX, MinX, TileList, _) ->
    TileList;

tiles_x(MinX, Index, TileList, MapData) ->
    
    Tile = array:get(Index, MapData),   
    io:fwrite("tiles_x: ~w ~w ~w~n", [Tile, MinX, Index]),
    
    tiles_x(MinX, Index - 1, [Tile | TileList], MapData).




