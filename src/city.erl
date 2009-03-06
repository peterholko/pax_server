%% Author: Peter
%% Created: Feb 4, 2009
%% Description: TODO: Add description to city
-module(city).
-behaviour(gen_server).

%%
%% Include files
%%

-include("schema.hrl").

%%
%% Exported Functions
%%
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-export([start/2, stop/1]).

-record(module_data, {
          city_id,  
          player_id, 
          self
         }).

%%
%% API Functions
%%

start(CityId, PlayerId) ->
	gen_server:start({global, {city, CityId}}, city, [CityId, PlayerId], []).

init([CityId, PlayerId]) 
  when is_integer(CityId),
       is_integer(PlayerId) ->
    process_flag(trap_exit, true),
    io:fwrite("city - city_id: ~w player_id: ~w~n", [CityId, PlayerId]),
    {ok, #module_data{ city_id = CityId, player_id = PlayerId, self = self() }}.

terminate(_Reason, _) ->
    ok.

stop(ProcessId) 
  when is_pid(ProcessId) ->
    gen_server:cast(ProcessId, stop).

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({'GET_STATE'}, _From, Data) ->
    [City] = db:read(city, Data#module_data.city_id),
    
	{reply, {City#city.id, City#city.player_id, 1, City#city.state, City#city.x, City#city.y}, Data};

handle_call({'GET_CITY_ID'}, _From, Data) ->
	{reply, Data#module_data.city_id, Data};

handle_call({'GET_PLAYER_ID'}, _From, Data) ->
	{reply, Data#module_data.player_id, Data};

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


            

