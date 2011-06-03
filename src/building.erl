%% Author: Peter
%% Created: Mar 17, 2009
%% Description: TODO: Add description to building
-module(building).

%%
%% Include files
%%

-include("common.hrl").
-include("schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

%%
%% Exported Functions
%%
-export([add_to_queue/2,
         calc_gold_cost/1,
         tuple_form/1,
         queue_tuple_form/1,
         process_production/3
         ]).

%%
%% API Functions
%%

tuple_form(Buildings) ->
    F = fun(Building, BuildingList) ->
            BuildingTuple = {Building#building.id, 
                             trunc(Building#building.hp), 
                             Building#building.type},
            [BuildingTuple | BuildingList]
        end,

    lists:foldl(F, [], Buildings).

queue_tuple_form(CityId) ->
    BuildingQueueList = db:dirty_index_read(building_queue, CityId, #building_queue.city_id),
    
    F = fun(BuildingQueue, TupleList) ->
            BuildingQueueTuple = {?TASK_BUILDING,                 
                                  BuildingQueue#building_queue.building_type,                 
                                  BuildingQueue#building_queue.id,
                                  BuildingQueue#building_queue.city_id,
                                  trunc(BuildingQueue#building_queue.production),
                                  BuildingQueue#building_queue.created_time},
            [BuildingQueueTuple | TupleList]
        end,

    lists:foldl(F, [], BuildingQueueList).

calc_gold_cost(Type) ->
    [BuildingType] = db:dirty_read(building_type, Type),
    BuildingType#building_type.gold_cost.

process_production(BuildingId, BuildingQueue, Assignment) ->
    log4erl:info("{~w} Process Production", [?MODULE]),
    [Building] = db:dirty_read(building, BuildingId),
    [BuildingType] = db:dirty_read(building_type, Building#building.type),

    CurrentTime = util:get_time_seconds(),
    LastUpdateTime = BuildingQueue#building_queue.last_update,
    NumGameDays = util:diff_game_days(LastUpdateTime, CurrentTime),

    Production = BuildingQueue#building_queue.production,
    CasteRate = caste:get_production_rate(Assignment#assignment.caste),
    NewProduction = Production + (CasteRate * NumGameDays * Assignment#assignment.amount),                    

    case NewProduction >= BuildingType#building_type.production_cost of
        true ->
            log4erl:info("{~w} Building Complete", [?MODULE]),
            NewBuildingHp = BuildingType#building_type.total_hp,
            NewBuilding = Building#building {hp = NewBuildingHp},
            
            db:dirty_write(NewBuilding),
            db:dirty_delete(building_queue, BuildingQueue#building_queue.id);
        false ->
            log4erl:info("{~w} Updating Production CurrentProduction: ~w", [?MODULE, NewProduction]),
            CompletionRatio = NewProduction / BuildingType#building_type.production_cost,
            NewBuildingHp = util:round3(BuildingType#building_type.total_hp * CompletionRatio),
            NewBuilding = Building#building {hp = NewBuildingHp},

            RoundedProduction = util:round3(NewProduction),
            NewBuildingQueue = BuildingQueue#building_queue {production = RoundedProduction,
                                                             last_update = CurrentTime},
            db:dirty_write(NewBuilding),
            db:dirty_write(NewBuildingQueue)
    end.
            
add_to_queue(CityId, BuildingType) ->
    CurrentTime = util:get_time_seconds(),   
    BuildingId = counter:increment(building),
    BuildingQueueId = counter:increment(building_queue),

    BuildingQueue = #building_queue {id = BuildingQueueId,
                                     city_id = CityId,                                   
                                     building_id = BuildingId,
                                     building_type = BuildingType,
                                     production = 0,
                                     created_time = CurrentTime,
                                     last_update = CurrentTime},

    Building = #building {id = BuildingId,
                          city_id = CityId,
                          type = BuildingType,
                          hp = 0},
   
    db:dirty_write(BuildingQueue),
    db:dirty_write(Building).
 
            
          
