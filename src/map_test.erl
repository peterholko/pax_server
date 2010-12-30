%% -------------------------------------------------------------------
%% Author  : Peter
%%% Description :
%%%
%%% Created : June 23, 2010
%%% -------------------------------------------------------------------
-module(map_test).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("schema.hrl").
-include("common.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([test/0]).
-record(module_data, {}).
%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    spawn(fun() -> gen_server:start({global, map_test}, map_test, [], []) end).

test() ->
    gen_server:cast({global, map_test}, start),
    gen_server:call({global, map_test}, map_list),
    gen_server:call({global, map_test}, map_get).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    Data = #module_data {},
    {ok, Data}.

handle_cast(start, Data) ->  
     
    map_port:start(),
    map_port:load(),
    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data}.


handle_call(map_list, _From, Data) ->
    MapList = get_map_tiles([203,204,205,253,254,255,303,304,305], []),
    {reply, MapList, Data};

handle_call(map_get, _From, Data) ->
    MapGet = map_port:get(254),
    {reply, MapGet, Data};

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
%% --------------------------------------------------------------------

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

