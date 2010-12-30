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

-record(module_data, {}).
%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, kingdom_pid}, kingdom, [], []).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    Data = #module_data {},
    {ok, Data}.

handle_cast('TEST', Data) ->   
    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({'GET_ARMIES', PlayerId}, _From, Data) ->
    [Kingdom] = db:dirty_read(kingdom, PlayerId), 
    {reply, Kingdom#kingdom.armies, Data};

handle_call({'GET_ARMIES_ID_PID', PlayerId}, _From, Data) ->
    [Kingdom] = db:dirty_read(kingdom, PlayerId),
    Armies = Kingdom#kingdom.armies,
    
    F = fun(ArmyId, Rest) -> [{ArmyId, global:whereis_name({army, ArmyId})} | Rest] end,
    ArmiesIdPid = lists:foldl(F, [], Armies),	
    {reply, ArmiesIdPid, Data};

handle_call({'GET_CITIES_ID_PID', PlayerId}, _From, Data) ->
    [Kingdom] = db:dirty_read(kingdom, PlayerId),
    Cities = Kingdom#kingdom.cities,
    
    F = fun(CityId, Rest) -> [{CityId, global:whereis_name({city, CityId})} | Rest] end,
    CitiesIdPid = lists:foldl(F, [], Cities),	
    {reply, CitiesIdPid, Data};

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

