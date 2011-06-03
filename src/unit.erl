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
-export([add_to_queue/3,
         is_valid_unit_type/1,
         calc_unit_cost/2,
         calc_retreat_time/1,
         get_unit_ids/2, 
         get_unit/2,
         highest_unit_movement/1,
         units_id/1, 
         tuple_form/1, 
         queue_tuple_form/1,
         process_production/2]).

%%
%% API Functions
%%
add_to_queue(CityId, UnitType, UnitSize) ->
    CurrentTime = util:get_time_seconds(),
    
    UnitQueue = #unit_queue {id = counter:increment(unit_queue),
                             city_id = CityId,
                             unit_type = UnitType,
                             unit_size = UnitSize,
                             production = 0,
                             created_time = CurrentTime,
                             last_update = CurrentTime},

    db:dirty_write(UnitQueue).    

queue_tuple_form(CityId) ->
    UnitQueueList = db:dirty_index_read(unit_queue, CityId, #unit_queue.city_id),
    
    F = fun(UnitQueue, TupleList) ->
            UnitQueueTuple = {?TASK_UNIT,
                              UnitQueue#unit_queue.unit_type,
                              UnitQueue#unit_queue.id,
                              UnitQueue#unit_queue.city_id,
                              trunc(UnitQueue#unit_queue.production),
                              UnitQueue#unit_queue.created_time},
            [UnitQueueTuple | TupleList]
        end,

    lists:foldl(F, [], UnitQueueList).

is_valid_unit_type(UnitType) ->
    case db:dirty_read(unit_type, UnitType) of
        [UnitType] ->
            Result = true;
        _ ->
            Result = false
    end,
    Result.

calc_unit_cost(UnitType, UnitSize) ->
    UnitTypeModifier = cost_by_unit_type(UnitType),
    UnitTypeModifier * UnitSize.
    
cost_by_unit_type(UnitTypeId) ->
    [UnitType] = db:dirty_read(unit_type, UnitTypeId),
    UnitType#unit_type.gold_cost.

calc_retreat_time(_ArmyId) ->
    10.

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

tuple_form(Units) ->
    F = fun(Unit, UnitList) ->
                io:fwrite("unit - units_tuple: ~w~n", [Unit]),
                UnitTuple = {Unit#unit.id, Unit#unit.type, Unit#unit.size},
                [UnitTuple | UnitList]
        end,
    
    lists:foldl(F, [], Units).

process_production(UnitQueue, Assignment) ->
    [UnitType] = db:dirty_read(unit_type, UnitQueue#unit_queue.unit_type),

    CurrentTime = util:get_time_seconds(),
    LastUpdateTime = UnitQueue#unit_queue.last_update,
    NumGameDays = util:diff_game_days(LastUpdateTime, CurrentTime),
    
    Production = UnitQueue#unit_queue.production,
    CasteRate = caste:get_production_rate(Assignment#assignment.caste),
    NewProduction = Production + (CasteRate * NumGameDays * Assignment#assignment.amount),
    
    case NewProduction >= UnitType#unit_type.production_cost of
        true ->
            log4erl:info("{~w} Unit Complete", [?MODULE]),
            
            Unit = #unit { id = counter:increment(unit),
                           entity_id = UnitQueue#unit_queue.city_id,
                           entity_type = ?OBJECT_CITY,
                           type = UnitQueue#unit_queue.unit_type,
                           size = UnitQueue#unit_queue.unit_size,
                           hp = UnitType#unit_type.total_hp},

            db:dirty_write(Unit),
            db:dirty_delete(unit_queue, UnitQueue#unit_queue.id);
        false ->
            log4erl:info("{~w} Updating Unit Production: ~w", [?MODULE, NewProduction]),
            
            RoundedProduction = util:round3(NewProduction),
            NewUnitQueue = UnitQueue#unit_queue {production = RoundedProduction,
                                                 last_update = CurrentTime},
            
            db:dirty_write(NewUnitQueue)
    end.   
                           
