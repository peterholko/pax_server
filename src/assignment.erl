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
-export([tuple_form/1]).
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
