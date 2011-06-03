%% Author: Peter
%% Created: March 5, 2011
%% Description: TODO: Add description to object
-module(reputation).

%%
%% Include files
%%

-include("common.hrl").
-include("schema.hrl").
%%
%% Exported Functions
%%
-export([get_value/2, set_value/3]).
-export([check_create_order/2]).
%%
%% API Functions
%%

get_value(TargetPlayerId, PlayerId) ->
    case db:dirty_read(reputation_ref, {TargetPlayerId, PlayerId}) of
        [ReputationRef] ->
            ReputationId = ReputationRef#reputation_ref.reputation_id,
            [Reputation] = db:dirty_read(reputation, ReputationId),
            Value = Reputation#reputation.value;
        _ ->
            Value = 0
    end,
    Value.

set_value(PlayerId, TargetPlayerId, NewValue) ->
    case db:dirty_read(reputation_ref, {PlayerId, TargetPlayerId}) of
        [ReputationRef] ->
            ReputationId = ReputationRef#reputation_ref.reputation_id,
            [Reputation] = db:dirty_read(reputation, ReputationId),
            NewReputation = Reputation#reputation { value = NewValue},
            db:dirty_write(NewReputation);
        _ ->
            ReputationId = counter:increment(reputation),
            Reputation = #reputation {id = ReputationId,
                                      player_id = PlayerId,
                                      target_player_id = TargetPlayerId,
                                      value = NewValue},
            ReputationRef = #reputation_ref {ref = {PlayerId, TargetPlayerId},
                                             reputation_id = ReputationId},
            db:dirty_write(Reputation),
            db:dirty_write(ReputationRef)
    end.

check_create_order(PlayerId, TargetPlayerId) ->
    TargetValue = get_value(TargetPlayerId, PlayerId),
    create_order(TargetValue). 
    
create_order(Value) when Value >= 3 -> true;
create_order(_Value) -> false.

%%
%% Local Functions
%%

