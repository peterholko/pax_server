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
-export([create/4, delete/1, set/2, transfer/3, get_by_type/3]).
-export([add/2, remove/2]).
-export([tuple_form/1, add_to_queue/6]).
-export([check_type/1, check_building_req/2, check_improvement_req/2]).
-record(module_data, {}).
%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, item_pid}, item, [], []).

create(EntityId, PlayerId, Type, Volume) ->
    gen_server:cast({global, item_pid}, {'CREATE_ITEM', EntityId, PlayerId, Type, Volume}).

delete(ItemId) ->
    gen_server:cast({global, item_pid}, {'DELETE_ITEM', ItemId}).

add(ItemId, Volume) ->
    update(ItemId, Volume).

remove(ItemId, Volume) ->
    update(ItemId, -1*Volume).

set(ItemId, Volume) ->
    gen_server:cast({global, item_pid}, {'SET_ITEM', ItemId, Volume}).

update(ItemId, Volume) ->
    gen_server:cast({global, item_pid}, {'UPDATE_ITEM', ItemId, Volume}).

transfer(ItemId, EntityId, PlayerId) ->
    gen_server:cast({global, item_pid}, {'TRANSFER_ITEM', ItemId, EntityId, PlayerId}).

split(ItemId, Volume) ->
    gen_server:cast({global, item_pid}, {'SPLIT_ITEM', ItemId, Volume}).

get_by_type(EntityId, PlayerId, Type) ->
    gen_server:call({global, item_pid}, {'GET_ITEM_BY_TYPE', EntityId, PlayerId, Type}).

tuple_form(Items) ->
    F = fun(Item, ItemList) ->
        {EntityId, PlayerId} = Item#item.ref,
        ItemTuple = {Item#item.id,
                     EntityId,
                     PlayerId,
                     Item#item.type,
                     Item#item.volume},
        [ItemTuple | ItemList]
    end,
    lists:foldl(F, [], Items).

check_type(ItemTypeId) ->
    case db:dirty_read(item_type, ItemTypeId) of
        [_ItemType] ->
            Result = true;
        _ ->
            Result = false
    end,
    Result.

check_improvement_req(ImprovementType, ItemTypeId) ->
    case db:dirty_read(item_type, ItemTypeId) of
        [ItemType] ->
            Result = ImprovementType =:= ItemType#item_type.improvement_req;
        _ ->
            Result = false
    end,
    Result.

check_building_req(BuildingType, ItemTypeId) ->
    case db:dirty_read(item_type, ItemTypeId) of
        [ItemType] ->
            Result = BuildingType =:= ItemType#item_type.building_req;
        _ ->
            Result = false
    end,
    Result.

add_to_queue(PlayerId, CityId, ContractType, TargetRef, ItemType, ItemSize) ->
    CurrentTime = util:get_time_seconds(),
    ContractId = counter:increment(contract),

    Contract = #contract {id = ContractId,
                          city_id = CityId,
                          type = ContractType,
                          target_ref = TargetRef,
                          object_type = ItemType,
                          production = 0,
                          created_time = CurrentTime,
                          last_update = CurrentTime},
   
    ItemQueue = #item_queue {contract_id = ContractId,
                             player_id = PlayerId,
                             item_type = ItemType,
                             item_size = ItemSize},

    db:dirty_write(Contract),
    db:dirty_write(ItemQueue).
    

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    Data = #module_data {},
    {ok, Data}.

handle_cast({'CREATE_ITEM', EntityId, PlayerId, Type, Volume},Data) ->  
    log4erl:info("{~w} Create Item", [?MODULE]),  
    create_item(EntityId, PlayerId, Type, Volume),
    {noreply, Data};

handle_cast({'DELETE_ITEM', ItemId}, Data) ->
    delete_item(ItemId),
    {noreply, Data};

handle_cast({'SET_ITEM', ItemId, Volume}, Data) ->
    set_item(ItemId, Volume),
    {noreply, Data};

handle_cast({'UPDATE_ITEM', ItemId, Volume}, Data) ->
    log4erl:info("UPDATE_ITEM"),  
    update_item(ItemId, Volume),
    {noreply,  Data};

handle_cast({'TRANSFER_ITEM', ItemId, EntityId, PlayerId}, Data) ->
    transfer_item(ItemId, EntityId, PlayerId),
    {noreply, Data};

handle_cast({'SPLIT_ITEM', ItemId, Volume}, Data) ->
    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({'GET_ITEM_BY_TYPE', EntityId, PlayerId, Type}, _From, Data) ->
    Item = get_item_by_type(EntityId, PlayerId, Type),
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
create_item(EntityId, PlayerId, Type, Volume) ->
    case db:dirty_read(item_type_ref, {EntityId, PlayerId, Type}) of
        [] ->
            new_item(EntityId, PlayerId, Type, Volume);
        [ItemTypeRef | _Rest] ->
            log4erl:info("ItemTypeRef: ~w", [ItemTypeRef]),
            update_item(ItemTypeRef#item_type_ref.item_id, Volume)
    end.

delete_item(ItemId) ->
    [Item] = db:dirty_read(item, ItemId),
    [ItemTypeRef] = db:dirty_index_read(item_type_ref, ItemId, #item_type_ref.item_id),
    mnesia:dirty_delete_object(item_type_ref, ItemTypeRef),
    db:dirty_delete(item, ItemId).

update_item(ItemId, Volume) ->
    log4erl:info("begin update_item"),
    [Item] = db:dirty_read(item, ItemId),
    CurrentVolume = Item#item.volume,
    NewItem = Item#item {volume = CurrentVolume + Volume},
    log4erl:info("update_item itemId: ~w", [Item#item.id]),
    db:dirty_write(NewItem).

transfer_item(ItemId, TargetEntityId, TargetPlayerId) ->
    [Item] = db:dirty_read(item, ItemId),
    {EntityId, PlayerId} = Item#item.ref,
    db:dirty_delete(item_type_ref, {EntityId, PlayerId, Item#item.type}),

    ItemRef = {TargetEntityId, TargetPlayerId},
    NewItem = Item#item { ref = ItemRef},

    ItemTypeRef = #item_type_ref {entity_type_ref = {TargetEntityId, TargetPlayerId, Item#item.type},                                
                                  item_id = ItemId},

    db:dirty_write(NewItem),
    db:dirty_write(ItemTypeRef).

set_item(ItemId, Volume) ->
    [Item] = db:dirty_read(item, ItemId),
    NewItem = Item#item {volume = Volume},
    db:dirty_write(NewItem).

new_item(EntityId, PlayerId, Type, Volume) ->
    log4erl:info("Begin new_item"),    
    ItemId = counter:increment(item) + 1000,
    Item = #item {id = ItemId,
                  ref = {EntityId, PlayerId},
                  type = Type,
                  volume = Volume},
    ItemTypeRef = #item_type_ref {entity_type_ref = {EntityId, PlayerId, Type},                               
                                  item_id = ItemId},

    log4erl:info("new_item itemId: ~w", [Item#item.id]),
    db:dirty_write(Item),
    db:dirty_write(ItemTypeRef).

get_item_by_type(EntityId, PlayerId, Type) ->
    ItemTypeRefList = db:dirty_read(item_type_ref, {EntityId, PlayerId, Type}),
    ItemList = get_item_list(ItemTypeRefList, []),
    item_list(ItemList).

item_list([]) ->
    {not_found};
item_list(ItemList) ->
    {found, ItemList}.

get_item_list([], ItemList) ->
    ItemList;

get_item_list([ItemTypeRef | Rest], ItemList) ->
    [Item] = db:dirty_read(item, ItemTypeRef#item_type_ref.item_id),
    
    NewItemList = [Item | ItemList],
    get_item_list(Rest, NewItemList).

