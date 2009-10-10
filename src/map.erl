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

-export([get_explored_map/1, get_surrounding_tiles/2, start/0, stop/1, convert_coords/1, convert_coords/2]).

-record(module_data, {
          map,   
          self
         }).

%%
%% API Functions
%%

get_explored_map(TileIndexList) ->
	gen_server:call(global:whereis_name(map_pid), {'GET_EXPLORED_MAP', TileIndexList}).

get_surrounding_tiles(X, Y) ->
    io:fwrite("map - get_surrounding_tiles x: ~w y: ~w~n",[X,Y]),
	Tiles2D = surrounding_tiles_2D(X, Y, 1),
	io:fwrite("map - get_surrounding_tiles Tiles2D: ~w~n",[Tiles2D]),
	TileList = surrounding_tiles(Tiles2D),
	io:fwrite("map - get_surrounding_tiles TileList: ~w~n",[TileList]),
	TileList.

start() ->
	gen_server:start({global, map_pid}, map, [], []).

init([]) ->
  	
	map_port:start(),
	map_port:load(),
	
    {ok, #module_data{ self = self() }}.

terminate(_Reason, _) ->
    ok.

stop(ProcessId) 
  when is_pid(ProcessId) ->
    gen_server:cast(ProcessId, stop).
 
handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({'GET_EXPLORED_MAP', TileIndexList}, _From, Data) ->    
  	MapTiles = get_map_tiles(TileIndexList, []),
     
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
      
get_map_tiles([], MapList) ->
    MapList;

get_map_tiles(TileIndexList, MapList) ->
    [TileIndex | Rest] = TileIndexList,   
    
    if
        TileIndex >= 0 ->
        	Tile = map_port:get(TileIndex),
			NewMapList = [{TileIndex, Tile} | MapList];
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
				



