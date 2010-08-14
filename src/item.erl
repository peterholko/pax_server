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
-export([add_item/3, set_item/2, update_item/2, get_item_by_type/2]).
-record(module_data, {}).
%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, item_pid}, item, [], []).

add_item(EntityId, Type, Value) ->
    gen_server:cast({global, item_pid}, {'ADD_ITEM', EntityId, Type, Value}).

set_item(ItemId, Value) ->
    gen_server:cast({global, item_pid}, {'SET_ITEM', ItemId, Value}).

update_item(ItemId, Value) ->
    gen_server:cast({global, item_pid}, {'UPDATE_ITEM', ItemId, Value}).

get_item_by_type(EntityId, Type) ->
    gen_server:call({global, item_pid}, {'GET_ITEM_BY_TYPE', EntityId, Type}).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    Data = #module_data {},
    {ok, Data}.

handle_cast({'ADD_ITEM', EntityId, Type, Value}, Data) ->   
    add_item_i(EntityId, Type, Value),
    {noreply, Data};

handle_cast({'SET_ITEM', ItemId, Value}, Data) ->
    set_item_i(ItemId, Value),
    {noreply, Data};

handle_cast({'UPDATE_ITEM', ItemId, Value}, Data) ->
    update_item_i(ItemId, Value),
    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({'GET_ITEM_BY_TYPE', EntityId, Type}, _From, Data) ->
    Item = get_item_by_type_i(EntityId, Type),
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
add_item_i(EntityId, Type, Value) ->
    case db:dirty_read(item_type_ref, {EntityId, Type}) of
        [ItemTypeRef] ->
            io:fwrite("ItemTypeRef: ~w~n", [ItemTypeRef]),
            update_item_i(ItemTypeRef#item_type_ref.item_id, Value);
        _ ->
            new_item_i(EntityId, Type, Value)
    end.

update_item_i(ItemId, Value) ->
    [Item] = db:dirty_read(item, ItemId),
    CurrentValue = Item#item.value,
    NewItem = Item#item {value = CurrentValue + Value},
    db:dirty_write(NewItem).

set_item_i(ItemId, Value) ->
    [Item] = db:dirty_read(item, ItemId),
    NewItem = Item#item {value = Value},
    db:dirty_write(NewItem).

new_item_i(EntityId, Type, Value) ->
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

get_item_by_type_i(EntityId, Type) ->
    case db:dirty_read(item_type_ref, {EntityId, Type}) of
        [ItemTypeRef] ->
            [Item] = db:dirty_read(item, ItemTypeRef#item_type_ref.item_id),
            Result = {found, Item};
        _ ->
            Result = {not_found}
    end,
    Result.            

