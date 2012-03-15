%% -------------------------------------------------------------------
%% Author  : Peter
%%% Description :
%%%
%%% Created : Feb, 2012
%%% -------------------------------------------------------------------
-module(army_manager).
-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("schema.hrl").
-include("packet.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([add/3]).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, army_manager_pid}, army_manager, [], []).

add(PlayerId, PlayerName, Socket) ->
    gen_server:cast({global, army_manager_pid}, {'ADD', PlayerId, PlayerName, Socket}).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    {ok, []}.

handle_cast('TEST', Data) ->   
    {noreply, Data};

handle_cast({'ADD', PlayerId, PlayerName, Socket}, Data) ->
    Member = #member {player_id = PlayerId,
                      player_name = PlayerName,
                      socket = Socket},

    NewMembers = [Member | Data#data.members],
    NewData = Data#data {members = NewMembers},

    {noreply, NewData};

handle_cast({'REMOVE', PlayerId}, Data) ->
    NewData = lists:keydelete(PlayerId, 1, Data#data.members), 
    {noreply, NewData};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

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
