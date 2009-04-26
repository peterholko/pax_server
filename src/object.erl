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
-export([get_object_atom/1]).

%%
%% API Functions
%%

get_object_atom(?OBJECT_ARMY) -> army;
get_object_atom(?OBJECT_CITY) -> city;
get_object_atom(Number) -> none.

%%
%% Local Functions
%%

