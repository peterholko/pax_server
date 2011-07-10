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
         is_valid/2,
         check_type/1
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

calc_gold_cost(Type) ->
    [BuildingType] = db:dirty_read(building_type, Type),
    BuildingType#building_type.gold_cost.

add_to_queue(CityId, BuildingType) ->
    CurrentTime = util:get_time_seconds(),   
    BuildingId = counter:increment(building),
    ContractId = counter:increment(contract),
    
    TargetRef = {BuildingId, ?CONTRACT_BUILDING},
    Contract = #contract {id = ContractId,
                          city_id = CityId,
                          target_ref = TargetRef,
                          object_type = BuildingType,
                          production = 0,
                          created_time = CurrentTime,
                          last_update = CurrentTime},


    BuildingQueue = #building_queue {contract_id = ContractId,
                                     building_id = BuildingId,
                                     building_type = BuildingType},

    Building = #building {id = BuildingId,
                          city_id = CityId,
                          type = BuildingType,
                          hp = 0},
    db:dirty_write(Contract),
    db:dirty_write(BuildingQueue),
    db:dirty_write(Building).

is_valid(_IsPlayer = true, _IsValidType = true) ->
    true;
is_valid(_IsPlayer = false, _IsValidType) ->
    {false, not_player};
is_valid(_IsPlayer, _IsValidType = false) ->
    {false, invalid_building_type}.
 
check_type(TypeId) ->
    case db:dirty_read(building_type, TypeId) of
        [_BuildingType] ->
            Result = true;
        _ ->
            Result = false
    end,
    Result.

get_building(CityId, BuildingType) ->
    Buildings = db:dirty_index_read(building, CityId, #building.city_id),

    lists:keyfind(BuildingType, #building.type, Buildings).
