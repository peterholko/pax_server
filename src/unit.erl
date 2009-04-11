%% Author: Peter
%% Created: Mar 1, 2009
%% Description: TODO: Add description to unit
-module(unit).

%%
%% Include files
%%

-include("common.hrl").
-include("schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%



%units_speed(ArmyId) ->
%	db:do(qlc:q([Y#unit_type.speed || X <- mnesia:table(army_units),
%                                         X#unit.army_id =:= ArmyId,
%                                         Y <- mnesia:table(unit_type),
%                                         X#unit.type_id =:= Y#unit_type.id])).

