%% Author: Peter
%% Created: Dec 26, 2008
%% Description: TODO: Add description to character
-module(item_category).

-include("schema.hrl").
-include("common.hrl").

-export([is_population/1, find_available_item/3]).

is_population(CategoryId) ->
    [ItemCategory] = db:dirty_read(item_category, CategoryId),

    case ItemCategory#item_category.name of
        human_citizen -> Return = {true, ?RACE_HUMAN};
        elf_citizen -> Return = {true, ?RACE_ELF};
        goblin_citizen -> Return = {true, ?RACE_GOLBIN};
        dwarf_citizen -> Return = {true, ?RACE_DWARF};
        _ -> Return = false
    end,
    Return.

find_available_item(CityId, CategoryId, TotalAmount) ->
    [ItemCategory] = db:dirty_read(item_category, CategoryId),
    match_item_contains(false, CityId, TotalAmount, ItemCategory#item_category.contains).

match_item_contains(Result, _CityId, _TotalAmount, _Contains) ->
    Result;

match_item_contains(false, CityId, TotalAmount, [ItemId | Rest]) ->
    ItemVolume = item:get_volume(ItemId),

    case ItemVolume >= TotalAmount of
        true -> Result = {true, ItemId};
        false = Result = false
    end,

    find_contains(Result, CityId, TotalAmount, Rest).

