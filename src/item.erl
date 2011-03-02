%% -------------------------------------------------------------------
%% Author  : Peter
%%% Description :
%%%
%%% Created : June 23, 2010
%%% -------------------------------------------------------------------
-module(item).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("schema.hrl").
-include("common.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([add/3, set/2, update/2, transfer/2, get_by_type/2]).
-export([items_tuple/1]).
-record(module_data, {}).
%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, item_pid}, item, [], []).

add(EntityId, Type, Value) ->
    gen_server:cast({global, item_pid}, {'ADD_ITEM', EntityId, Type, Value}).

set(ItemId, Value) ->
    gen_server:cast({global, item_pid}, {'SET_ITEM', ItemId, Value}).

update(ItemId, Value) ->
    gen_server:cast({global, item_pid}, {'UPDATE_ITEM', ItemId, Value}).

transfer(ItemId, EntityId) ->
    gen_server:cast({global, item_pid}, {'TRANSFER_ITEM', ItemId, EntityId}).

get_by_type(EntityId, Type) ->
    gen_server:call({global, item_pid}, {'GET_ITEM_BY_TYPE', EntityId, Type}).

items_tuple(Items) ->
    F = fun(Item, ItemList) ->
        ItemTuple = {Item#item.id,
                     Item#item.entity_id,
                     Item#item.type,
                     Item#item.value},
        [ItemTuple | ItemList]
    end,
    lists:foldl(F, [], Items).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    Data = #module_data {},
    {ok, Data}.

handle_cast({'ADD_ITEM', EntityId, Type, Value}, Data) ->   
    add_item(EntityId, Type, Value),
    {noreply, Data};

handle_cast({'SET_ITEM', ItemId, Value}, Data) ->
    set_item(ItemId, Value),
    {noreply, Data};

handle_cast({'UPDATE_ITEM', ItemId, Value}, Data) ->
    update_item(ItemId, Value),
    {noreply, Data};

handle_cast({'TRANSFER_ITEM', ItemId, EntityId}, Data) ->
    transfer_item(ItemId, EntityId),
    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({'GET_ITEM_BY_TYPE', EntityId, Type}, _From, Data) ->
    Item = get_item_by_type(EntityId, Type),
    {reply, Item, Data};

handle_call(Event, From, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
                              {line, ?LINE},
                              {self, self()}, 
                              {message, Event}, 
                              {from, From}
                             ]),
    {noreply, Data}.

handle_info(Info, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
                              {line, ?LINE},
                              {self, self()}, 
                              {message, Info}]),
    {noreply, Data}.

code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.

terminate(_Reason, _) ->
    ok.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
add_item(EntityId, Type, Value) ->
    case db:dirty_read(item_type_ref, {EntityId, Type}) of
        [ItemTypeRef] ->
            io:fwrite("ItemTypeRef: ~w~n", [ItemTypeRef]),
            update_item(ItemTypeRef#item_type_ref.item_id, Value);
        _ ->
            new_item(EntityId, Type, Value)
    end.

update_item(ItemId, Value) ->
    [Item] = db:dirty_read(item, ItemId),
    CurrentValue = Item#item.value,
    NewItem = Item#item {value = CurrentValue + Value},
    db:dirty_write(NewItem).

transfer_item(ItemId, EntityId) ->
    [Item] = db:dirty_read(item, ItemId),
    NewItem = Item#item { entity_id = EntityId},
    db:dirty_write(NewItem).

set_item(ItemId, Value) ->
    [Item] = db:dirty_read(item, ItemId),
    NewItem = Item#item {value = Value},
    db:dirty_write(NewItem).

new_item(EntityId, Type, Value) ->
    ItemId = counter:increment(item),
    ItemRef = {EntityId, Type},
    Item = #item {id = ItemId,
                  entity_id = EntityId,
                  type = Type,
                  value = Value},
    ItemTypeRef = #item_type_ref {entity_type_ref = ItemRef,
                                  item_id = ItemId},
    db:dirty_write(Item),
    db:dirty_write(ItemTypeRef).

get_item_by_type(EntityId, Type) ->
    case db:dirty_read(item_type_ref, {EntityId, Type}) of
        [ItemTypeRef] ->
            [Item] = db:dirty_read(item, ItemTypeRef#item_type_ref.item_id),
            Result = {found, Item};
        _ ->
            Result = {not_found}
    end,
    Result.            

