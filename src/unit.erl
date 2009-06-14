%% Author: Peter
%% Created: Mar 1, 2009
%% Description: TODO: Add description to unit
-module(unit).

%%
%% Include files
%%

-include("common.hrl").
-include("schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

%%
%% Exported Functions
%%
-export([init_units/2, get_unit/2,
		 units_id/1, units_tuple/1, units_queue_tuple/1]).

%%
%% API Functions
%%

init_units([], DictUnits) ->
    DictUnits;

init_units(ListUnits, DictUnits) ->
    [Unit | Rest] = ListUnits,
    NewDictUnits = dict:store(Unit#unit.id, Unit, DictUnits),
	init_units(Rest, NewDictUnits).

get_unit(UnitId, Units) ->
    UnitResult = dict:is_key(UnitId, Units),
    
    if
        UnitResult ->         
            Unit = dict:fetch(UnitId, Units);
        true ->
            Unit = false
    end,
    Unit.

units_id([]) ->
    [];
    
units_id(Units) ->
    F = fun(Unit, UnitList) ->
                UnitId = Unit#unit.id,
                [UnitId | UnitList]
        end,
    
    lists:foldl(F, [], Units).


units_tuple([]) ->
    [];

units_tuple(Units) ->
	
	F = fun({UnitId, Unit}, UnitList) ->
				io:fwrite("unit - units_tuple: ~w~n", [Unit]),
				UnitTuple = {UnitId, Unit#unit.type, Unit#unit.size},
				[UnitTuple | UnitList]
		end,
	
	UnitsList = dict:to_list(Units),
	lists:foldl(F, [], UnitsList).

units_queue_tuple([]) ->
	[];
units_queue_tuple(UnitsQueue) ->
    
    F = fun(UnitQueue, UnitQueueList) ->
                UnitQueueTuple = {UnitQueue#unit_queue.id, 
                                  UnitQueue#unit_queue.unit_type, 
                                  UnitQueue#unit_queue.unit_size,
                                  UnitQueue#unit_queue.start_time,
                                  UnitQueue#unit_queue.end_time},
                [UnitQueueTuple | UnitQueueList]
        end,
    
    lists:foldl(F, [], UnitsQueue).

