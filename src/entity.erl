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
-export([get_player_id/1, on_same_tile/3]).
-export([get_type/1]).
%%
%% API Functions
%%

get_type(TargetPid) ->
    gen_server:call(TargetPid, {'GET_TYPE'}).

get_player_id(TargetPid) ->
    gen_server:call(TargetPid, {'GET_PLAYER_ID'}).

on_same_tile(TargetPid, X, Y) ->
    gen_server:call(TargetPid, {'ON_SAME_TILE', X, Y}).

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



