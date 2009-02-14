%% Author: Peter
%% Created: Feb 8, 2009
%% Description: TODO: Add description to entity
-module(entity).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([entity_list/1, add_entities/2]).

%%
%% API Functions
%%

entity_list(PlayerId) ->
    %Get Player's entities
    Armies = db:select_army(PlayerId),
    Cities = db:select_city(PlayerId),
                 
    EntityList = lists:append(Armies, Cities),
    EntityList.

add_entities(EntityList, []) ->
	EntityList;
  
add_entities(EntityList, NewEntities) ->
	
	[NewEntity | RestEntities ] = NewEntities,
	NewEntityList = [NewEntity | EntityList],

	add_entities(NewEntityList, RestEntities).

%%
%% Local Functions
%%


	
