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
-export([check_queue/1,
         buildings_tuple/1, 
         buildings_queue_tuple/1]).

%%
%% API Functions
%%

buildings_tuple([]) ->
    [];

buildings_tuple(Buildings) ->
    F = fun(Building, BuildingList) ->
            BuildingTuple = {Building#building.id, 
                             trunc(Building#building.hp), 
                             Building#building.type},
            [BuildingTuple | BuildingList]
        end,

    lists:foldl(F, [], Buildings).

buildings_queue_tuple([]) ->
    [];

buildings_queue_tuple(BuildingsQueue) ->
    F = fun(BuildingQueue, BuildingQueueList) ->
            BuildingQueueTuple = {BuildingQueue#building_queue.id,
                                  BuildingQueue#building_queue.building_id,
                                  trunc(BuildingQueue#building_queue.production),
                                  BuildingQueue#building_queue.start_time},
            [BuildingQueueTuple | BuildingQueueList]
        end,
    lists:foldl(F, [], BuildingsQueue).

check_queue(CityId) ->
    log4erl:info("{~w} Checking Queue", [?MODULE]),
    BuildingsQueue = db:dirty_index_read(building_queue, CityId, #building_queue.city_id),
    Assignments = db:dirty_match_object({assignment, '_', CityId, '_', '_', '_', ?TASK_CONSTRUCTION}),

    F = fun(BuildingQueue) ->
            check_assignment(BuildingQueue, Assignments)            
        end,

    lists:foreach(F, BuildingsQueue).

check_assignment(BuildingQueue, Assignments) ->
    F = fun(Assignment) ->
            TaskId = Assignment#assignment.task_id,
            BuildingId = BuildingQueue#building_queue.id,
            case TaskId =:= BuildingId of
                true ->
                    log4erl:info("{~w} Processing Production", [?MODULE]),
                    process_production(BuildingId, BuildingQueue, Assignment);
                false ->
                    nothing
            end                
        end,

    lists:foreach(F, Assignments).

process_production(BuildingId, BuildingQueue, Assignment) ->
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
            

            
            
            


