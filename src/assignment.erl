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
-export([tuple_form/1, remove_all/1, is_valid_task/3, add/6]).
%%
%% API Functions
%%

%%
%% Local Functions
%%
tuple_form(Assignments) ->
    F = fun(Assignment, AssignmentList) ->
            {TargetId, TargetType} = Assignment#assignment.target_ref,
            AssignmentTuple = {Assignment#assignment.id,
                               Assignment#assignment.caste,
                               Assignment#assignment.race,
                               Assignment#assignment.amount,
                               TargetId,
                               TargetType},
            [AssignmentTuple | AssignmentList]
        end,
    lists:foldl(F, [], Assignments).

add(CityId, Caste, Race, Amount, TaskId, TaskType) ->
    case db:dirty_match_object({assignment, '_', CityId, Caste, Race, '_', {TaskId, TaskType}}) of
        [Assignment] ->
            NewAmount = Assignment#assignment.amount + Amount,
            NewAssignment = Assignment#assignment { amount = NewAmount},
            AssignmentId = NewAssignment#assignment.id;
        _ ->    
            AssignmentId = counter:increment(assignment),
            NewAssignment = #assignment {  id = AssignmentId,
                                           city_id = CityId,
                                           caste = Caste,
                                           race = Race,
                                           amount = Amount,
                                           target_ref = {TaskId, TaskType}}
    end,
    db:dirty_write(NewAssignment),
    AssignmentId.
 
is_valid_task(CityId, ImprovementId, ?OBJECT_IMPROVEMENT) ->
    case db:dirty_read(improvement, ImprovementId) of
        [Improvement] ->
            case Improvement#improvement.city_id =:= CityId of
                true ->
                    Result = true;
                false ->
                    Result = false
            end;
        _ ->
            Result = false
    end,
    Result;
is_valid_task(CityId, BuildingId, ?OBJECT_BUILDING) ->
    case db:dirty_read(building, BuildingId) of
        [Building] ->
            case Building#building.city_id =:= CityId of
                true ->
                    Result = true;
                false ->
                    Result = false
            end;
        _ ->
            Result = false
    end,
    Result;
is_valid_task(_CityId, _TaskId, _TaskType) ->
    false.

remove_all(TargetRef) ->
    Assignments = db:dirty_index_read(assignment, TargetRef , #assignment.target_ref),

    F = fun(Assignment) ->
            db:dirty_delete_object(Assignment)
        end,

    lists:foreach(F, Assignments).
