%% Author: Peter
%% Created: Feb 8, 2009
%% Description: TODO: Add description to entity
-module(entity).

%%
%% Include files
%%

-include("schema.hrl").

%%
%% Exported Functions
%%
-export([player_id/1, type/1, add_entity/3]).
-export([entity_list/1, add_entities/2]).
-export([get_player_id/1, on_same_tile/3]).
-export([get_type/2]).
%%
%% API Functions
%%

player_id(EntityId) ->
    [Entity] = db:read(entity, EntityId),
    Entity#entity.player_id.

type(EntityId) ->
    [Entity] = db:read(entity, EntityId),
    Entity#entity.type.

add_entity(EntityId, PlayerId, Type) ->
    Entity = #entity {id = EntityId,
                      player_id = PlayerId,
                      type = Type},
    db:write(Entity).

get_type(TargetPid, TargetId) ->
    gen_server:call(TargetPid, {'GET_TYPE', TargetId}).

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



