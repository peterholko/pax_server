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
          city, 
          units, 
          units_queue,
          player_id, 
          self,
          save_city = false
         }).

%%
%% API Functions
%%

start(CityId, PlayerId) ->
    case db:read(city, CityId) of 
        [City] ->
            gen_server:start({global, {city, CityId}}, city, [City, PlayerId], []);
		Any ->
    		{error, Any}
    end.

init([City, PlayerId]) 
  when is_tuple(City),
       is_integer(PlayerId) ->
    process_flag(trap_exit, true),
    
    io:fwrite("city - city_id: ~w player_id: ~w~n", [City#city.id, PlayerId]),
    ListUnits = db:index_read(unit, City#city.id, #unit.entity_id),
    DictUnits = dict:new(),
    NewDictUnits = unit:init_units(ListUnits, DictUnits),
	
    UnitQueue = db:index_read(unit_queue, City#city.id, #unit_queue.city_id),
    
    {ok, #module_data{ city = City, units = NewDictUnits, units_queue = UnitQueue, player_id = PlayerId, self = self() }}.

terminate(_Reason, _) ->
    ok.

stop(ProcessId) 
  when is_pid(ProcessId) ->
    gen_server:cast(ProcessId, stop).

handle_cast({'ADD_UNIT', Unit}, Data) ->
    
    Units = Data#module_data.units,
    NewUnits = [Unit | Units],
    NewData = Data#module_data {units = NewUnits},
    
    {noreply, NewData};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({'QUEUE_UNIT', PlayerId, UnitType, UnitSize}, _From, Data) ->
    
    City = Data#module_data.city,
    UnitsQueue = Data#module_data.units_queue,
    
    if
        City#city.player_id =:= PlayerId ->
            NewUnitsQueue = queue_unit(City#city.id, UnitsQueue, UnitType, UnitSize),
            NewData = Data#module_data { units_queue = NewUnitsQueue},
            Result = {city, queued_unit};
        true ->
            NewData = Data,
            Result = {city, error}
	end,
    
    {reply, Result, NewData};

handle_call({'GET_INFO', PlayerId}, _From, Data) ->

    City = Data#module_data.city,

	if 
		City#city.player_id =:= PlayerId ->  
            %Get UnitsQueue and Units
            UnitsQueue = Data#module_data.units_queue,
            Units = Data#module_data.units,
            
            %Check if any units are completed
            {NewUnitsQueue, UnitsToAdd} = check_queue(UnitsQueue),
            
            %Add any units from the queue
            NewUnits = add_units_from_queue(Units, UnitsToAdd),
            
            %Assign any changes to module data
            NewData = Data#module_data {units = NewUnits, units_queue = NewUnitsQueue},
            
            %Convert record to tuple packet form
            UnitsTuple = unit:units_tuple(NewUnits),
            UnitsQueueTuple = unit:units_queue_tuple(NewUnitsQueue),
                              
			CityInfo = {detailed, City#city.buildings, UnitsTuple, UnitsQueueTuple};
		true ->
            NewData = Data,
			CityInfo = {generic, City#city.player_id}
	end,

    io:fwrite("city - CityInfo: ~w~n", [CityInfo]),
	{reply, CityInfo , NewData};

handle_call({'TRANSFER_UNIT', UnitId, TargetId, TargetAtom}, _From, Data) ->
    
    io:fwrite("city - transfer unit.~n"),
    
    Units = Data#module_data.units,
    UnitResult = dict:is_key(UnitId, Units),
    
    if
        UnitResult =/= false ->
            Unit = dict:fetch(UnitId, Units),
			
            case gen_server:call(global:whereis_name({TargetAtom, TargetId}), {'RECEIVE_UNIT', Unit, Data#module_data.player_id}) of
                {receive_unit, success} ->
                    NewUnits = dict:erase(UnitId, Units),
                    NewData = Data#module_data {units = NewUnits, save_city = true},
                    TransferUnitInfo = {transfer_unit, success};
                Error ->
                    TransferUnitInfo = Error,
                    NewData = Data
            end;
        true ->
            TransferUnitInfo = {transfer_unit, error},
            NewData = Data
    end,         		
    
	{reply, TransferUnitInfo , NewData};
                
handle_call({'RECEIVE_UNIT', Unit, PlayerId}, _From, Data) ->
    
    io:fwrite("city - receive unit.~n"),
    
    City = Data#module_data.city,
    Units = Data#module_data.units,
    
    if
        PlayerId =:= Data#module_data.player_id ->
            %Check to see if unit already exists
            UnitResult = dict:is_key(Unit#unit.id, Units),
            
            if
                UnitResult =:= false ->
                    
                    NewUnit = Unit#unit {entity_id = City#city.id},
                    NewUnits = dict:store(Unit#unit.id, NewUnit, Units),
                    NewData = Data#module_data {units = NewUnits, save_city = true},
            		ReceiveUnitInfo = {receive_unit, success};
                
                true ->
    				NewData = Data,
                    ReceiveUnitInfo = {receive_unit, error}
            end;               
        true ->
			NewData = Data,
            ReceiveUnitInfo = {receive_unit, error}
    end,
    
    {reply, ReceiveUnitInfo, NewData};   

handle_call({'GET_STATE'}, _From, Data) ->
    City = Data#module_data.city,
	{reply, {City#city.id, City#city.player_id, ?OBJECT_CITY, City#city.state, City#city.x, City#city.y}, Data};

handle_call({'GET_ID'}, _From, Data) ->
    City = Data#module_data.city,
	{reply, City#city.id, Data};

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
  
queue_unit(CityId, UnitsQueue, UnitType, UnitSize) ->
    CurrentTime = util:get_time_seconds(),
	StartTime = get_queue_unit_time(UnitsQueue, CurrentTime),   
	io:fwrite("city - db_queue_unit - StartTime: ~w~n", [StartTime]),
    
	UnitQueue = #unit_queue {id = counter:increment(unit_queue),
                             city_id = CityId,
                             unit_type = UnitType,
                             unit_size = UnitSize,
                             start_time = StartTime,
                             end_time = StartTime + 30},
    
    [UnitQueue | UnitsQueue].

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

check_queue([]) ->
    {[], []};

check_queue(QueueInfo) ->
    CurrentTime = util:get_time_seconds(),

    io:fwrite("city - check_queue - CurrentTime: ~w~n", [CurrentTime]),  
    
    F = fun(UnitQueue, UnitsToRemove) ->
                EndTime = UnitQueue#unit_queue.end_time,
                
                if
                    EndTime =< CurrentTime ->    
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
    {NewQueueInfo, UnitsToRemove}.  

add_units_from_queue(Units, []) ->
    Units;

add_units_from_queue(Units, UnitsToAdd)->
    
    [UnitQueue | Rest] = UnitsToAdd,
    
    Unit = #unit {id = counter:increment(unit),
                  entity_id = UnitQueue#unit_queue.city_id,
                  entity_type = ?OBJECT_CITY,
                  type = UnitQueue#unit_queue.unit_type,
                  size = UnitQueue#unit_queue.unit_size},
    
	NewUnits = dict:store(Unit#unit.id, Unit, Units),
    add_units_from_queue(NewUnits, Rest).
    



          
                        


                
                 
                

