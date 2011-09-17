%% Author: Peter
%% Created: March 5, 2011
%% Description: TODO: Add description to object
-module(claim).

%%
%% Include files
%%

-include("common.hrl").
-include("schema.hrl").
%%
%% Exported Functions
%%
-export([tuple_form/1, info/1, complete/1, cancel/1, check_status/1, is_valid/4]).
%%
%% API Functions
%%

tuple_form(Claims) ->
    F = fun(Claim, ClaimList) ->
            ClaimTuple = {Claim#claim.id, 
                          Claim#claim.tile_index, 
                          Claim#claim.city_id,
                          Claim#claim.state,
                          Claim#claim.created_time},
            [ClaimTuple | ClaimList]
        end,
    lists:foldl(F, [], Claims).

info(TileIndex) ->
    case db:dirty_index_read(claim, TileIndex, #claim.tile_index) of
        [Claim] ->
            ClaimTuple = {Claim#claim.id, 
                          Claim#claim.tile_index, 
                          Claim#claim.city_id, 
                          Claim#claim.state,
                          Claim#claim.created_time};
        _ ->
            ClaimTuple = {-1, -1, -1, -1, -1}
    end,
    ClaimTuple.

complete(ClaimId) ->
    ?INFO("Completed Claim Id", ClaimId),
    case db:dirty_read(claim, ClaimId) of
        [Claim] ->
            NewClaim = Claim#claim { army_id = -1,
                                     state = ?STATE_COMPLETED},
            db:dirty_write(NewClaim);
        _ ->
            ?INFO("Cannot find claim: ", ClaimId)
    end.

cancel(ArmyId) ->
    case db:dirty_index_read(claim, ArmyId, #claim.army_id) of
        [Claim] ->
            db:dirty_delete(Claim);
        _ ->
            ?INFO("Claim not found for army id: ", ArmyId)
    end.

check_status(TileIndex) ->
    case db:dirty_index_read(claim, TileIndex, #claim.tile_index) of
        [Claim] ->
            case Claim#claim.state of
                ?STATE_COMPLETED ->
                    Result = true;
                _ ->
                    Result = false
            end;
        _ ->
            Result = false
    end,
    Result.

%Valid X, Valid Y, Exists, MaxReached
is_valid(true, true, false, false) ->
    true;
is_valid(_, _, _, _) ->
    ?INFO("Add claim failed."),
    false.

