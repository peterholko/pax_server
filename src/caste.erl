%% Author: Peter
%% Created: Sep 26, 2009
%% Description: TODO: Add description to population
-module(caste).

%%
%% Include files
%%
-include("common.hrl").
%%
%% Exported Functions
%%
-export([get_production_rate/1]).

%%
%% API Functions
%%

get_production_rate(?CASTE_SLAVE) ->
    ?CASTE_SLAVE_RATE;
get_production_rate(?CASTE_SOLDIER) ->
    ?CASTE_SOLDIER_RATE;
get_production_rate(?CASTE_COMMONER) ->
    ?CASTE_COMMONER_RATE;
get_production_rate(?CASTE_NOBLE) ->
    ?CASTE_NOBLE_RATE.

%%
%% Local Functions
%%

