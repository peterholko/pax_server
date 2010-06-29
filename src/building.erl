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
-export([calc_building_time/4,
         buildings_tuple/1, 
         buildings_queue_tuple/1]).

%%
%% API Functions
%%

calc_building_time(BuildingTypeId, CompletionRate, _Caste, Amount) ->
    case db:dirty_read(building_type, BuildingTypeId) of
        [BuildingType] ->
            TimeCompleted = BuildingType#building_type.building_time * CompletionRate,
            TimeLeft = BuildingType#building_type.building_time - TimeCompleted,
            NewTimeLeft = TimeLeft / Amount,
            NewBuildingTime = round(TimeCompleted + NewTimeLeft);
        _ ->            
            log4erl:error("~w: Invalid BuildingTypeId ~w", [?MODULE,BuildingTypeId]),
            erlang:error("Invalid BuildingTypeId"),
            NewBuildingTime = ?MAX_INT
    end,
    NewBuildingTime.

buildings_tuple([]) ->
    [];

buildings_tuple(Buildings) ->
    F = fun(Building, BuildingList) ->
            BuildingTuple = {Building#building.id, Building#building.type},
            [BuildingTuple | BuildingList]
        end,

    lists:foldl(F, [], Buildings).

buildings_queue_tuple([]) ->
    [];

buildings_queue_tuple(BuildingsQueue) ->
    F = fun(BuildingQueue, BuildingQueueList) ->
            BuildingQueueTuple = {BuildingQueue#building_queue.id,
                                  BuildingQueue#building_queue.building_type,
                                  BuildingQueue#building_queue.start_time,
                                  BuildingQueue#building_queue.end_time},
            [BuildingQueueTuple | BuildingQueueList]
        end,
    lists:foldl(F, [], BuildingsQueue).
            
