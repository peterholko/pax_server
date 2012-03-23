%% Author: Peter
%% Created: Apr 19, 2009
%% Description: TODO: Add description to object
-module(object).

%%
%% Include files
%%

-include("common.hrl").

%%
%% Exported Functions
%%
-export([get_atom/1, get_pid/2]).

%%
%% API Functions
%%

get_atom(?OBJECT_ARMY) -> army;
get_atom(?OBJECT_CITY) -> city;
get_atom(_) -> none.

get_pid(army, ArmyId) -> global:whereis_name({army, ArmyId});
get_pid(city, CityId) -> global:whereis_name({city, CityId}).

%%
%% Local Functions
%%

