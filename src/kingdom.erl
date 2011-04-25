%% -------------------------------------------------------------------
%% Author  : Peter
%%% Description :
%%%
%%% Created : June 23, 2010
%%% -------------------------------------------------------------------
-module(kingdom).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("schema.hrl").
-include("common.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([get_gold/1, add_gold/2, remove_gold/2]).
-export([get_name/1, get_info_kingdom/1]).
-export([get_cities/1, get_cities_id_pid/1]).

-record(module_data, {}).
%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, kingdom_pid}, kingdom, [], []).

get_gold(PlayerId) ->
    gen_server:call({global, kingdom_pid}, {'GET_GOLD', PlayerId}).

add_gold(PlayerId, Amount) ->
    update_gold(PlayerId, Amount).

remove_gold(PlayerId, Amount) ->
    update_gold(PlayerId, -1*Amount).

update_gold(PlayerId, Amount) ->
    gen_server:cast({global, kingdom_pid}, {'UPDATE_GOLD', PlayerId, Amount}).

get_cities_id_pid(PlayerId) ->
    gen_server:call({global, kingdom_pid}, {'GET_CITIES_ID_PID', PlayerId}).

get_name(PlayerId) ->
    [Kingdom] = db:dirty_index_read(kingdom, PlayerId, #kingdom.player_id),
    Kingdom#kingdom.name.

get_info_kingdom(PlayerId) ->
    [Kingdom] = db:dirty_index_read(kingdom, PlayerId, #kingdom.player_id),
    {Kingdom#kingdom.id, Kingdom#kingdom.name, Kingdom#kingdom.gold}.

get_cities(PlayerId) ->
    [Kingdom] = db:dirty_index_read(kingdom, PlayerId, #kingdom.player_id),
    Kingdom#kingdom.cities.

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    Data = #module_data {},
    {ok, Data}.

handle_cast('TEST', Data) ->   
    {noreply, Data};

handle_cast({'UPDATE_GOLD', PlayerId, Amount}, Data) ->
    [Kingdom] = db:dirty_index_read(kingdom, PlayerId, #kingdom.player_id), 
    NewKingdom = Kingdom#kingdom { gold = Kingdom#kingdom.gold + Amount},
    db:dirty_write(NewKingdom),
    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({'GET_ARMIES', PlayerId}, _From, Data) ->
    [Kingdom] = db:dirty_index_read(kingdom, PlayerId, #kingdom.player_id), 
    {reply, Kingdom#kingdom.armies, Data};

handle_call({'GET_ARMIES_ID_PID', PlayerId}, _From, Data) ->
    [Kingdom] = db:dirty_index_read(kingdom, PlayerId, #kingdom.player_id), 
    Armies = Kingdom#kingdom.armies,
    
    F = fun(ArmyId, Rest) -> [{ArmyId, global:whereis_name({army, ArmyId})} | Rest] end,
    ArmiesIdPid = lists:foldl(F, [], Armies),	
    {reply, ArmiesIdPid, Data};

handle_call({'GET_CITIES_ID_PID', PlayerId}, _From, Data) ->
    [Kingdom] = db:dirty_index_read(kingdom, PlayerId, #kingdom.player_id), 
    Cities = Kingdom#kingdom.cities,
    
    F = fun(CityId, Rest) -> [{CityId, global:whereis_name({city, CityId})} | Rest] end,
    CitiesIdPid = lists:foldl(F, [], Cities),	
    {reply, CitiesIdPid, Data};

handle_call({'GET_GOLD', PlayerId}, _From, Data) ->
    [Kingdom] = db:dirty_index_read(kingdom, PlayerId, #kingdom.player_id), 
    {reply, Kingdom#kingdom.gold, Data};

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

