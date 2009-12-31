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
-export([buildings_tuple/1, buildings_queue_tuple/1]).

%%
%% API Functions
%%

buildings_tuple([]) ->
    [];

buildings_tuple(Buildings) ->
    F = fun(Building, BuildingList) ->
            BuildingTuple = {Building#building.id, Building#building.type},
            [UnitTuple | UnitList]
        end,

    lists:foldl(F, [], Units).

buildings_queue_tuple([]) ->
    [];

buildings_queue_tuple(BuildingsQueue) ->
    F = fun(BuildingQueue, BuildingQueuList) ->
            BuildingQueueTuple = {BuildingQueue#building_queue.id,
                                  BuildingQueue#building_queue.type,
                                  BuildingQueue#building_queue.start_time,
                                  BuildingQueue#building_queue.end_time),
            [BuildingQueueTuple | BuildingQueueList]
        end,
    lists:foldl(F, [], BuildingsQueue).
            
