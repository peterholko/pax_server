%% Author: Peter
%% Created: Jan 9, 2012
%% Description: TODO: Add description to city_lib
-module(structure).

%%
%% Include files
%%

-include("game.hrl").
-include("common.hrl").
-include("schema.hrl").

%%
%% Exported Functions
%%
-export([is_available/2, type/1, remove_cost/4]).

%%
%% API Functions
%%

is_available(SourceId, ?OBJECT_IMPROVEMENT) ->
    improvement:is_available(SourceId);

is_available(SourceId, ?OBJECT_BUILDING) ->
    building:is_available(SourceId);

is_available(_SourceId, _SourceType) ->
    false.

remove_cost(building, EntityId, PlayerId, BuildingTypeId) ->
    [BuildingType] = db:dirty_read(building_type, BuildingTypeId),
    GoldCost = BuildingType#building_type.gold_cost,
    StoneCost = BuildingType#building_type.stone_cost,
    LumberCost = BuildingType#building_type.lumber_cost,

    remove_generic_cost(EntityId, PlayerId, GoldCost, StoneCost, LumberCost);

remove_cost(improvement, EntityId, PlayerId, ImprovementTypeId) ->
    [ImprovementType] = db:dirty_read(improvement_type, ImprovementTypeId),
    GoldCost = ImprovementType#improvement_type.gold_cost,
    StoneCost = ImprovementType#improvement_type.stone_cost,
    LumberCost = ImprovementType#improvement_type.lumber_cost,

    remove_generic_cost(EntityId, PlayerId, GoldCost, StoneCost, LumberCost).

remove_generic_cost(EntityId, PlayerId, GoldCost, StoneCost, LumberCost) ->
    StoneList = item:get_stone({?OBJECT_CITY, EntityId}, PlayerId),
    LumberList = item:get_lumber({?OBJECT_CITY, EntityId}, PlayerId),

    case check_cost(PlayerId, StoneList, LumberList, GoldCost, StoneCost, LumberCost) of
        true ->
            kingdom:remove_gold(PlayerId, GoldCost),
            remove_material(StoneList, StoneCost),
            remove_material(LumberList, LumberCost),
            Result = true;
        false ->
            Result = false
    end,
    Result.    

remove_material(_List, 0) ->
    done;

remove_material([Material | Rest], MaterialCost) ->
    if
        Material#item.volume >= MaterialCost ->
            NewMaterialCost = 0,
            item:remove(Material#item.id, MaterialCost);
        true ->
            NewMaterialCost = MaterialCost - Material#item.volume,
            item:remove(Material#item.id, Material#item.volume)
    end,
    remove_material(Rest, NewMaterialCost).

check_cost(PlayerId, StoneList, LumberList, GoldCost, StoneCost, LumberCost) ->
    Gold = check_gold(PlayerId, GoldCost),
    Stone = check_stone(StoneList, StoneCost),
    Lumber = check_lumber(LumberList, LumberCost),

    io:fwrite("G: ~w S: ~w L: ~w~n", [Gold, Stone, Lumber]),

    Gold and Stone and Lumber.

check_gold(PlayerId, GoldCost) ->
    Gold = kingdom:get_gold(PlayerId),
    Gold >= GoldCost.

check_stone(StoneList, StoneCost) ->
    F = fun(Stone, Total) ->
            Stone#item.volume + Total
        end,

    StoneTotal = lists:foldl(F, 0, StoneList),
    StoneCost < StoneTotal.

check_lumber(LumberList, LumberCost) ->
    F = fun(Lumber, Total) ->
            Lumber#item.volume + Total
        end,

    LumberTotal = lists:foldl(F, 0, LumberList),
    LumberCost < LumberTotal.

type(Structure) when is_record(Structure, improvement) ->
    Structure#improvement.type;

type(Structure) when is_record(Structure, building) ->
    Structure#building.type;

type(_Structure) ->
    -1.
    



%%
%% Local Functions
%%

