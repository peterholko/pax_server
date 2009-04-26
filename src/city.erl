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

-export([db_queue_unit/3]).

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

handle_call({'QUEUE_UNIT', PlayerId, UnitType, UnitSize}, _From, Data) ->
    
    case db:read(city, Data#module_data.city_id) of
        [City] ->
            if
                City#city.player_id =:= PlayerId ->
                    db_queue_unit(Data#module_data.city_id, UnitType, UnitSize),
                    Result = {city, queued_unit};
                true ->
                    Result = {city, error}
            end;
        _ ->
            Result = {city, error}
    end,   
    
    {reply, Result, Data};

handle_call({'GET_INFO', PlayerId}, _From, Data) ->
	
	case db:read(city, Data#module_data.city_id) of
		[City] ->
            io:fwrite("city - City: ~w~n", [City]),
			if 
				City#city.player_id =:= PlayerId ->
                    
                    %Check if any units are completed
                    UnitsQueueInfo = get_unit_queue(Data#module_data.city_id),
                    UnitsInfo = unit:db_units_info(Data#module_data.city_id),             
                    
                    %Convert record to tuple packet form
                    UnitsInfoTuple = unit:units_tuple(UnitsInfo),
                    UnitsQueueInfoTuple = unit:units_queue_tuple(UnitsQueueInfo),
                    
					CityInfo = {detailed, City#city.buildings, UnitsInfoTuple, UnitsQueueInfoTuple};
				true ->
					CityInfo = {generic, City#city.player_id}
			end;
		_ ->
			CityInfo = {none}
	end,

    io:fwrite("city - CityInfo: ~w~n", [CityInfo]),
	{reply, CityInfo , Data};

handle_call({'TRANSFER_UNIT', UnitId, TargetId, TargetAtom}, _From, Data) ->
    
    TransferUnitInfo = unit:transfer(Data#module_data.city_id, UnitId, TargetId, TargetAtom),
	{reply, TransferUnitInfo , Data};

handle_call({'GET_STATE'}, _From, Data) ->
    [City] = db:dirty_read(city, Data#module_data.city_id),
	{reply, {City#city.id, City#city.player_id, ?OBJECT_CITY, City#city.state, City#city.x, City#city.y}, Data};

handle_call({'GET_ID'}, _From, Data) ->
	{reply, Data#module_data.city_id, Data};

handle_call({'GET_PLAYER_ID'}, _From, Data) ->
	{reply, Data#module_data.player_id, Data};

handle_call({'GET_TYPE'}, _From, Data) ->
    {reply, ?OBJECT_CITY, Data};

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
  
get_queue_unit_time([], CurrentTime) ->
    CurrentTime;

get_queue_unit_time(QueueInfo, CurrentTime) ->
    io:fwrite("city - get_queue_unit_time - QueueInfo: ~w~n", [QueueInfo]),
    SortedQueueInfo = lists:keysort(5, QueueInfo),    
    LastUnitQueue = lists:last(SortedQueueInfo),  
	EndTime = LastUnitQueue#unit_queue.end_time,
    
    if
    	EndTime > CurrentTime ->
	        StartTime = EndTime;
        true ->
        	StartTime = CurrentTime
    end,
    StartTime.

get_unit_queue(CityId) ->
    UnitsQueueInfo = db_units_queue_info(CityId),
    check_queue(UnitsQueueInfo).

check_queue([]) ->
    [];

check_queue(QueueInfo) ->
    CurrentTime = util:get_time_seconds(),

    F = fun(UnitQueue, UnitsToRemove) ->
                EndTime = UnitQueue#unit_queue.end_time,
                
                if
                    EndTime =< CurrentTime ->
                        db_move_unit_queue(UnitQueue),
                        io:fwrite("city - check_queue - UnitQueue: ~w~n", [UnitQueue]),    
                        NewUnitsToRemove = [UnitQueue | UnitsToRemove];
                    true ->
                        NewUnitsToRemove = UnitsToRemove
                end,
                
        		NewUnitsToRemove
        end,
    
    UnitsToRemove = lists:foldl(F, [], QueueInfo),
    
    NewQueueInfo = QueueInfo -- UnitsToRemove,
    io:fwrite("city - check_queue - UnitsRemoved: ~w~n", [UnitsToRemove]),    
    NewQueueInfo.

db_units_queue_info(CityId) ->
    db:index_read(unit_queue, CityId, #unit_queue.city_id).

db_queue_unit(CityId, UnitType, UnitSize) ->

	F = fun() ->
    			CurrentTime = util:get_time_seconds(),
    			QueueInfo = mnesia:index_read(unit_queue, CityId, #unit_queue.city_id),
    
    			StartTime = get_queue_unit_time(QueueInfo, CurrentTime),   
                io:fwrite("city - db_queue_unit - StartTime: ~w~n", [StartTime]),
    
    			UnitQueue = #unit_queue {id = counter:increment(unit_queue), 
                      					 city_id = CityId,
                      					 unit_type = UnitType,
                      					 unit_size = UnitSize,
										 start_time = StartTime,
                      					 end_time = StartTime + 30},
                mnesia:write(UnitQueue)
        end,

	mnesia:transaction(F).

db_move_unit_queue(UnitQueue) ->
    
    F = fun() ->
                mnesia:delete(unit_queue, UnitQueue#unit_queue.id, write),
                
                Unit = #unit {id = counter:increment(unit),
                              entity_id = UnitQueue#unit_queue.city_id,
                              entity_type = ?OBJECT_CITY,
                              type = UnitQueue#unit_queue.unit_type,
                              size = UnitQueue#unit_queue.unit_size},
                
                mnesia:write(Unit)
        end,
    
    mnesia:transaction(F).

                
                        


                
                 
                

