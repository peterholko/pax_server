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
                    UnitInfo = db_unit_info(Data#module_data.city_id),                    
					CityInfo = {detailed, City#city.buildings, UnitInfo};
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

db_unit_info(CityId) ->
	db:do(qlc:q([{X#city_unit.id, Y#unit_type.id, X#city_unit.size, 
                  X#city_unit.start_time, X#city_unit.end_time} || X <- mnesia:table(city_unit),
															 X#city_unit.city_id =:= CityId,
                                                             Y <- mnesia:table(unit_type),
                                                             X#city_unit.type_id =:= Y#unit_type.id])).

db_queue_unit(CityId, UnitType, UnitSize) ->
    %TODO: Wrap in transaction
    CurrentTime = util:get_time_seconds(),
    UnitsInfo = db_unit_info(CityId), 
    SortedUnits = lists:keysort(5, UnitsInfo),    
    LastUnit = lists:last(SortedUnits),  
	{_, _, _, _, EndTime} = LastUnit,
    
    if
        EndTime > CurrentTime ->
            StartTime = EndTime;
        true ->
            StartTime = CurrentTime
    end,
    
    Unit = #city_unit {id = counter:increment(unit), 
                       city_id = CityId,
                       type_id = UnitType,
                       size = UnitSize,
                       start_time = StartTime,
                       end_time = StartTime + 1000},
    
    db:write(Unit).
                       
                       
            

