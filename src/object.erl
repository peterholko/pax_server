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
get_atom(?OBJECT_BATTLE) -> battle;
get_atom(?OBJECT_UNIT) -> unit;
get_atom(_) -> none.

get_pid(army, ArmyId) -> global:whereis_name({army, ArmyId});
get_pid(city, CityId) -> global:whereis_name({city, CityId});
get_pid(battle, BattleId) -> global:whereis_name({battle, BattleId});
get_pid(unit, UnitId) -> global:whereis_name(unit_pid).
%%
%% Local Functions
%%

