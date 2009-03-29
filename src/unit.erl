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
%-export([units_speed/1, unit_info/1]).

%%
%% API Functions
%%

units_speed(ArmyId) ->
	db:do(qlc:q([Y#unit_type.speed || X <- mnesia:table(unit),
                                         X#unit.army_id =:= ArmyId,
                                         Y <- mnesia:table(unit_type),
                                         X#unit.type_id =:= Y#unit_type.id])).

unit_info(ArmyId) ->	
	db:do(qlc:q([{X#unit.id, Y#unit_type.name, X#unit.size} || X <- mnesia:table(unit),
															 X#unit.army_id =:= ArmyId,
                                                             Y <- mnesia:table(unit_type),
                                                             X#unit.type_id =:= Y#unit_type.id])).

	


%%
%% Local Functions
%%

