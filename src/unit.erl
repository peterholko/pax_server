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
-export([is_valid_unit_type/1,
         calc_new_unit_cost/2,
         get_unit_ids/2, 
         get_unit/2,
         highest_unit_movement/1,
         units_id/1, 
         units_tuple/1, 
         units_queue_tuple/1]).

%%
%% API Functions
%%

is_valid_unit_type(UnitType) ->
    case db:dirty_read(unit_type, UnitType) of
        [UnitType] ->
            Result = true;
        _ ->
            Result = false
    end,
    Result.

calc_new_unit_cost(UnitType, UnitSize) ->
    UnitTypeModifier = cost_by_unit_type(UnitType),
    UnitTypeModifier * UnitSize.
    
cost_by_unit_type(UnitTypeId) ->
    [UnitType] = db:dirty_read(unit_type, UnitTypeId),
    UnitType#unit_type.cost.

get_unit_ids([], UnitIds) ->
    UnitIds;

get_unit_ids(ListUnits, UnitIds) ->
    [Unit | Rest] = ListUnits,
    NewUnitIds = [Unit#unit.id | UnitIds],
    get_unit_ids(Rest, NewUnitIds).

get_unit(UnitId, Units) ->
    io:fwrite("Units: ~w UnitId: ~w~n", [Units, UnitId]),
    case gb_sets:is_member(UnitId, Units) of
        true ->
            [Unit] = db:dirty_read(unit, UnitId);
        false ->
            Unit = false
    end,
    Unit.

highest_unit_movement(ArmyId) ->
    Units = db:dirty_index_read(unit, ArmyId, #unit.entity_id), 
    F = fun(Unit, UnitList) ->
            [UnitType] = db:dirty_read(unit_type, Unit#unit.type),
            UnitSpeed = UnitType#unit_type.movement,
            [UnitSpeed | UnitList]
        end,

    UnitSpeeds = lists:foldl(F, [], Units),
    lists:max(UnitSpeeds).

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
    
    F = fun(Unit, UnitList) ->
                io:fwrite("unit - units_tuple: ~w~n", [Unit]),
                UnitTuple = {Unit#unit.id, Unit#unit.type, Unit#unit.size},
                [UnitTuple | UnitList]
        end,
    
    lists:foldl(F, [], Units).

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

