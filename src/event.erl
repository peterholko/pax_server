%% -------------------------------------------------------------------
%% Author  : Peter
%%% Description : Event Manager
%%%
%%% Created : Nov 18, 2009
%%% -------------------------------------------------------------------
-module(event).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("common.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, event_pid}, event, [], []).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    {ok, none}.

handle_cast({'PROCESS_EVENT', EventTick, _EventData, ?EVENT_GROWTH}, Data) ->
    log4erl:info("Processing Event Growth"),  
 
    Cities = game:get_cities(),

    F = fun({_CityId, CityPid}) ->            
            gen_server:cast(CityPid, {'PROCESS_EVENT', EventTick, _EventData, ?EVENT_GROWTH})                
        end,
    
    lists:foreach(F, Cities),      

    game:add_event(self(), ?EVENT_GROWTH, none, ?GROWTH_TICK),
    
    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({'NONE'}, _From, Data) ->
    
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

