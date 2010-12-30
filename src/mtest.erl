-module(mtest).
-export([loop/0, start/0]).

start() ->
    map_port:start(),
    map_port:load(),
    get_map_tiles([203,204,205,253,254,255,303,304,305], []),
    map_port:get(254).
%    P = spawn(fun() -> loop() end),
%    P ! {start},
%    timer:sleep(500),
%    P ! {tilelist, []},
%    timer:sleep(500),
%    P ! {tile, 1}.

loop() ->
    receive
        {start} ->
            map_port:start(),
            map_port:load(),
            loop();
        {tilelist, TileList} ->
            get_map_tiles([203,204,205,253,254,255,303,304,305], []),
            loop();
        {tile, TileIndex} ->
            io:fwrite("Hello World~n"),
            MapTile = map_port:get(254),
            loop();
        Error ->
            io:fwrite("Error: ~w~n", [Error]),
            loop()
    end.

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
            
