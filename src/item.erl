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
-export([speed1/1, speed2/1,test1/1, test2/1]).
-record(module_data, {}).
%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, item_pid}, item, [], []).

add_item(EntityId, Type, Value) ->
    gen_server:cast({global, item_pid}, {'ADD_ITEM', EntityId, Type, Value}).

new_item(EntityId, Type, Value) ->
    gen_server:cast({global, item_pid}, {'NEW_ITEM', EntityId, Type, Value}).

speed1(N) ->
    mnesia:clear_table(item),
    {T1, _} = timer:tc(item, test1, [N]),
    io:fwrite("Test1: ~w~n", [T1]).

speed2(N) ->
    mnesia:clear_table(item),
    {T2, _} = timer:tc(item, test2, [N]),
    io:fwrite("Test2: ~w~n", [T2]).

test1(0) ->
    ok;

test1(N) ->
    new_item_tran(N, N, N),
    test1(N - 1).

test2(0) ->
    ok;

test2(N) ->
    new_item(N, N, N),
    test2(N - 1).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    Data = #module_data {},
    {ok, Data}.

handle_cast({'ADD_ITEM', EntityId, Type, Value}, Data) ->   
    add_item_internal(EntityId, Type, Value),
    {noreply, Data};

handle_cast({'NEW_ITEM', EntityId, Type, Value}, Data) ->
    new_item_internal(EntityId, Type, Value),
    {noreply, Data};

handle_cast({'SET_ITEM', ItemId, Value}, Data) ->
    set_item_internal(ItemId, Value),
    {noreply, Data};

handle_cast({'UPDATE_ITEM', ItemId, Value}, Data) ->
    update_item_internal(ItemId, Value),
    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({'GET_ITEM_BY_TYPE', EntityId, Type}, _From, Data) ->
    Type = get_item_by_type_internal(EntityId, Type),
    {reply, Type, Data};

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
add_item_internal(EntityId, Type, Value) ->
    case db:dirty_read(item_type_ref, {EntityId, Type}) of
        [ItemTypeRef] ->
            io:fwrite("ItemTypeRef: ~w~n", [ItemTypeRef]),
            update_item_internal(ItemTypeRef#item_type_ref.item_id, Value);
        _ ->
            new_item_internal(EntityId, Type, Value)
    end.

update_item_internal(ItemId, Value) ->
    [Item] = db:dirty_read(item, ItemId),
    CurrentValue = Item#item.value,
    NewItem = Item#item {value = CurrentValue + Value},
    db:dirty_write(NewItem).

set_item_internal(ItemId, Value) ->
    [Item] = db:dirty_read(item, ItemId),
    NewItem = Item#item {value = Value},
    db:dirty_write(NewItem).

new_item_internal(EntityId, Type, Value) ->
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

get_item_by_type_internal(EntityId, Type) ->
    case db:dirty_read(item_type_ref, {EntityId, Type}) of
        [ItemTypeRef] ->
            [Item] = db:dirty_read(item, ItemTypeRef#item_type_ref.item_id),
            Result = {found, Item};
        _ ->
            Result = {not_found}
    end,
    Result.            

new_item_tran(CityId, Type, Value) ->
    F = fun() ->
            ItemId = counter:increment(item),
            ItemRef = {CityId, Type},
            Item = #item {id = ItemId,
                          entity_id = CityId,
                          type = Type,
                          value = Value},
            ItemTypeRef = #item_type_ref {entity_type_ref = ItemRef,
                                          item_id = ItemId},
            mnesia:write(Item),
            mnesia:write(ItemTypeRef)
        end,
    {atomic, _Status} = mnesia:transaction(F).

















