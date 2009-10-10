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
-export([init_buildings/2]).

%%
%% API Functions
%%

init_buildings([], DictBuildings) ->
    DictBuildings;

init_buildings(ListBuildings, DictBuildings) ->
    [Building | Rest] = ListBuildings,
    NewDictBuildings = dict:store(Building#building.id, Building, DictBuildings),
	init_buildings(Rest, NewDictBuildings).


  
