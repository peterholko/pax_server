%% -------------------------------------------------------------------
%% Author  : Peter
%%% Description :
%%%
%%% Created : Feb, 2012
%%% -------------------------------------------------------------------
-module(city_manager).
-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("schema.hrl").
-include("packet.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([player_id/1]).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, city_manager_pid}, city_manager, [], []).

player_id(CityId) ->
    gen_server:call({global, city_manager_pid}, {'PLAYER_ID', CityId}).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    {ok, []}.

handle_cast('TEST', Data) ->   
    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({'PLAYER_ID', CityId}, _From, Data) ->
    [City] = db:dirty_read(city, CityId),
    {reply, City#city.player_id, Data};

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
