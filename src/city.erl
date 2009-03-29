%% Author: Peter
%% Created: Feb 4, 2009
%% Description: TODO: Add description to city
-module(city).
-behaviour(gen_server).

%%
%% Include files
%%

-include("common.hrl").
-include("schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

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

handle_call({'GET_INFO', PlayerId}, _From, Data) ->
	
	case db:read(city, Data#module_data.city_id) of
		[City] ->
            io:fwrite("city - City: ~w~n", [City]),
			if 
				City#city.player_id =:= PlayerId ->
                    
                    LandQueue = get_queue(Data#module_data.player_id, ?BUILDING_UNIT_LAND),
                    SeaQueue = get_queue(Data#module_data.player_id, ?BUILDING_UNIT_SEA),
                    AirQueue = get_queue(Data#module_data.player_id, ?BUILDING_UNIT_AIR),
                    
					CityInfo = {detailed, City#city.buildings, LandQueue, SeaQueue, AirQueue};
				true ->
					CityInfo = {generic, City#city.player_id}
			end;
		_ ->
			CityInfo = {none}
	end,

    io:fwrite("city - CityInfo: ~w~n", [CityInfo]),
	{reply, CityInfo , Data};

handle_call({'GET_STATE'}, _From, Data) ->
    [City] = db:dirty_read(city, Data#module_data.city_id),
	{reply, {City#city.id, City#city.player_id, ?OBJECT_CITY, City#city.state, City#city.x, City#city.y}, Data};

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










check_queue([], _, _) ->
    [];

check_queue(Queue, PlayerId, BuildingType) ->
    [UnitQueue | _] = Queue,
    {UnitQueueId, UnitType, UnitAmount, StartTime, BuildTime} = UnitQueue,
    
    io:fwrite("city - check_queue: ~w~n", [UnitQueue]),
    
    CurrentTime = util:now_to_milliseconds(erlang:now()),
    FinishTime = StartTime + BuildTime,
    
    if
        CurrentTime >= FinishTime ->
            remove_unit_queue(UnitQueueId),
            add_unit(UnitType, UnitAmount),
        	NewQueue = db_queue_info(PlayerId, BuildingType);
        true ->
            NewQueue = Queue,
            ok
    end,
    
    NewQueue.

remove_unit_queue(UnitQueueId) ->
    db:delete(unit_queue, UnitQueueId).

add_unit(UnitType, UnitAmount) ->
    ok.

get_queue(PlayerId, BuildingType) -> 
    Queue = db_queue_info(PlayerId, BuildingType),
	NewQueue = check_queue(Queue, PlayerId, BuildingType),
    NewQueue.

db_queue_info(PlayerId, BuildingType) ->
	db:do(qlc:q([{X#unit_queue.id,
                  X#unit_queue.unit_type, X#unit_queue.unit_amount, 
                  X#unit_queue.start_time, X#unit_queue.build_time} || X <- mnesia:table(unit_queue),
                                                                       X#unit_queue.player_id =:= PlayerId,
                                                                       X#unit_queue.building_type =:= BuildingType])).


            

