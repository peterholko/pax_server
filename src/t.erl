%% -------------------------------------------------------------------
%% Author  : Peter
%%% Description :
%%%
%%% Created : Oct 11, 2009
%%% -------------------------------------------------------------------
-module(t).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("packet.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start/1, login/0, build_farm/0, add_claim/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(data, {socket}).

%% ====================================================================
%% External functions
%% ====================================================================

start(Socket) ->
    gen_server:start({global, test_sender}, t, [Socket], []).

login() ->
    gen_server:cast(global:whereis_name(test_sender), 'LOGIN').

build_farm() ->
    gen_server:cast(global:whereis_name(test_sender), {'BUILD_IMPROVEMENT', 11, 5, 5, ?IMPROVEMENT_FARM}).

add_claim() ->
    gen_server:cast(global:whereis_name(test_sender), {'ADD_CLAIM', 11, 5, 5}).

%% ====================================================================
%% Server functions
%% ====================================================================

init([Socket]) ->
    Data = #data {socket = Socket},
    {ok, Data}.


handle_cast({'ADD_CLAIM', CityId, X, Y}, Data) ->
    AddClaim = #add_claim {city_id = CityId, x = X, y = Y},
    packet:send(Data#data.socket, AddClaim),
    
    {noreply, Data};

handle_cast({'BUILD_IMPROVEMENT', CityId, X, Y, ImprovementType}, Data) ->
    BuildImprovement = #build_improvement {city_id = CityId, x = X, y = Y, improvement_type = ImprovementType},
    packet:send(Data#data.socket, BuildImprovement),

    {noreply, Data};

handle_cast('LOGIN', Data) ->
    gen_tcp:send(Data#data.socket, <<?CMD_LOGIN, 4:16, "test", 6:16, "123123">>),
    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({'UPDATE_PERCEPTION'}, _From, Data) ->
    
    
    {reply, ok, Data};

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

