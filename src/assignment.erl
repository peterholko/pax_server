%% Author: Peter
%% Created: March 5, 2011
%% Description: TODO: Add description to object
-module(assignment).

%%
%% Include files
%%

-include("common.hrl").
-include("schema.hrl").
%%
%% Exported Functions
%%
-export([tuple_form/1,
         process_assignments/1]).
%%
%% API Functions
%%

process_assignments(Assignments) ->
    check_assignment(Assignments).

%%
%% Local Functions
%%
tuple_form(Assignments) ->
    F = fun(Assignment, AssignmentList) ->
            AssignmentTuple = {Assignment#assignment.id,
                               Assignment#assignment.caste,
                               Assignment#assignment.race,
                               Assignment#assignment.amount,
                               Assignment#assignment.task_id,
                               Assignment#assignment.task_type},
            [AssignmentTuple | AssignmentList]
        end,
    lists:foldl(F, [], Assignments).

check_assignment([]) ->
    done;

check_assignment([Assignment | Rest]) ->
    process_type(Assignment#assignment.task_type, Assignment),    

    check_assignment(Rest).

process_type(?TASK_BUILDING, Assignment) ->
    log4erl:info("{~w} - Process TASK_BUILDING type",[?MODULE]),
    BuildingId = Assignment#assignment.task_id,
    case db:dirty_index_read(building_queue, BuildingId, #building_queue.building_id) of
        [BuildingQueue] ->
            log4erl:info("{~w} - Found BuildingQueue ~w", [?MODULE, BuildingQueue]),
            building:process_production(BuildingId, BuildingQueue, Assignment);
        _ ->
            log4erl:info("{~w} - TASK_BUILDING nothing to produce", [?MODULE])
    end;

process_type(?TASK_UNIT, Assignment) ->
    CityId = Assignment#assignment.city_id,
    case db:dirty_index_read(unit_queue, CityId, #unit_queue.city_id) of
        [UnitQueue] ->
            unit:process_production(UnitQueue, Assignment);
        _ ->
            log4erl:info("{~w} - TASK_UNIT nothing to produce", [?MODULE])
    end.
        

    

