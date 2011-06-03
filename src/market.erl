%% -------------------------------------------------------------------
%% Author  : Peter
%%% Description :
%%%
%%% Created : March 1, 2011
%%% -------------------------------------------------------------------
-module(market).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("schema.hrl").
-include("common.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([create_sell_order/2, create_buy_order/5, fill_sell_order/3, fill_buy_order/3]).
-record(module_data, {}).
%% ====================================================================
%% External functions
%% ====================================================================

create_sell_order(ItemId, Price) ->
    gen_server:cast(global:whereis_name(market_pid), {'CREATE_SELL_ORDER', ItemId, Price}).

create_buy_order(CityId, PlayerId, ItemType, Volume, Price) ->
    gen_server:cast(global:whereis_name(market_pid), {'CREATE_BUY_ORDER', CityId, PlayerId, ItemType, Volume, Price}).

fill_sell_order(PlayerId, OrderId, Volume) ->
    gen_server:call(global:whereis_name(market_pid), {'FILL_SELL_ORDER', PlayerId, OrderId, Volume}). 

fill_buy_order(PlayerId, OrderId, Volume) ->
    gen_server:call(global:whereis_name(market_pid), {'FILL_BUY_ORDER', PlayerId, OrderId, Volume}).

start() ->
    gen_server:start({global, market_pid}, market, [], []).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    Data = #module_data {},
    {ok, Data}.

handle_cast({'CREATE_SELL_ORDER', ItemId, Price}, Data) ->   
    OrderId = counter:increment(market_order),    
    [Item] = db:dirty_read(item, ItemId),
    {EntityId, PlayerId} = Item#item.ref,
    MarketOrder = #market_order {id = OrderId,
                                 city_id = EntityId,
                                 player_id = PlayerId,                               
                                 item_id = ItemId,
                                 item_type = Item#item.type,
                                 item_volume = Item#item.volume,
                                 price = Price,
                                 type = ?MARKET_SELL,
                                 start_time = 0,
                                 duration = 0},
    db:dirty_write(MarketOrder),
    
    MarketItem = #market_item {id = Item#item.id,
                               ref = Item#item.ref,
                               type = Item#item.type,
                               volume = Item#item.volume},

    db:dirty_write(MarketItem),
    item:delete(Item#item.id),

    {noreply, Data};

handle_cast({'CREATE_BUY_ORDER', CityId, PlayerId, ItemType, Volume, Price}, Data) ->
    OrderId = counter:increment(market_order),
    MarketOrder = #market_order {id = OrderId,
                                 city_id = CityId,
                                 player_id = PlayerId,                               
                                 item_id = -1,
                                 item_type = ItemType,
                                 item_volume = Volume,
                                 price = Price,
                                 type = ?MARKET_SELL,
                                 start_time = 0,
                                 duration = 0},
    db:dirty_write(MarketOrder),

    {noreply, Data};  

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({'GET_ORDERS', CityId}, _From, Data) ->   

    Orders = get_orders_by_routes(CityId),
    {reply, Orders, Data};

%handle_call({'GET_ORDERS_BY_TYPE', CityId, Type}, _From, Data) ->   

    %Orders = get_orders_by_routes(CityId),
    %SortedOrders = lists:keysort(5, Orders),
    %{reply, Orders, Data};

handle_call({'FILL_SELL_ORDER', PlayerId, OrderId, Volume}, _From, Data) ->    
    case db:dirty_read(market_order, OrderId) of
        [MarketOrder] ->
            Return = process_sell_price(MarketOrder, PlayerId, Volume);
        _ ->
            Return = {error, invalid_order}
    end,                            
            
    {reply, Return, Data};

handle_call({'FILL_BUY_ORDER', PlayerId, OrderId, Volume} , _From, Data) ->    
    case db:dirty_read(market_order, OrderId) of
        [MarketOrder] ->
            Return = process_buy_order(MarketOrder, PlayerId, Volume);
        _ ->
            Return = {error, invalid_order}
    end,                            
            
    {reply, Return, Data};


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

get_orders_by_routes(CityId) ->
    case db:dirty_read(trade_route, CityId) of
        [TradeRoute] ->
            ListCities = TradeRoute#trade_route.cities,
            ListAllCities = ListCities ++ [CityId],
            Orders = get_orders_by_cities(ListAllCities, []);            
        _ ->
            Orders = []
    end,
    Orders.

get_orders_by_cities([], Orders) ->
    Orders;

get_orders_by_cities([CityId | RestCities], Orders) ->

    Order = db:dirty_index_read(market_order, CityId, #market_order.city_id),
    NewOrders = [Order | Orders],    

    get_orders_by_cities(RestCities, NewOrders).

process_sell_price(MarketOrder, PlayerId, Volume) ->     
    TotalPrice = Volume * MarketOrder#market_order.price,
    case compare_gold(kingdom:get_gold(PlayerId), TotalPrice) of
        true ->
            Return = process_sell_order(MarketOrder, PlayerId, Volume);
        false ->
            Return = {error, insufficient_gold}
    end,
    Return.

process_sell_order(MarketOrder, BuyerId, Volume) ->
    SellerId = MarketOrder#market_order.player_id,    
    [MarketItem] = db:dirty_read(market_item, MarketOrder#market_order.item_id),

    case compare_volume(MarketOrder#market_order.item_volume, Volume) of
        greater ->                        
            MarketItemVolume = MarketItem#market_item.volume,
            NewMarketItem = MarketItem#market_item { volume = MarketItemVolume - Volume},
            NewMarketOrder = MarketOrder#market_order {item_volume = MarketItemVolume - Volume},
            
            transfer_gold(MarketOrder, BuyerId, SellerId, Volume), 
               
            %Create new item for buyer
            item:create(MarketOrder#market_order.city_id, BuyerId, MarketOrder#market_order.item_type, Volume),
            %Update seller's item
            market_item_update(NewMarketItem),                       
            %Update market order
            update_order(NewMarketOrder),

            Return = {success, order_filled};
        equal ->
            transfer_gold(MarketOrder, BuyerId, SellerId, Volume),

            %Create new item for buyer
            item:create(MarketOrder#market_order.city_id, BuyerId, MarketOrder#market_order.item_type, Volume),            
            %Delete seller's item
            market_item_remove(MarketItem#market_item.id),
            %Delete market order
            remove_order(MarketOrder),

            Return = {success, order_filled};
        less ->       
            FillVolume = MarketOrder#market_order.item_volume,

            transfer_gold(MarketOrder, BuyerId, SellerId, Volume),

            %Create new item for buyer
            item:create(MarketOrder#market_order.city_id, BuyerId, MarketOrder#market_order.item_type, FillVolume),            
            %Delete seller's item
            market_item_remove(MarketItem#market_item.id),
            %Delete market order
            remove_order(MarketOrder),

            Return = {success, order_filled}                     
    end,
    Return.
       
process_buy_order(MarketOrder, SellerId, SelectedVolume) ->
    BuyerId = MarketOrder#market_order.player_id,    
    log4erl:info("~w: CityId ~w SellerId ~w Type ~w", [?MODULE, 
                                                                 MarketOrder#market_order.city_id,
                                                                 SellerId,
                                                                 MarketOrder#market_order.item_type]),

    case item:get_by_type(MarketOrder#market_order.city_id,
                          SellerId,
                          MarketOrder#market_order.item_type) of
        {found, Item} ->
            ItemVolume = select_volume(SelectedVolume, Item#item.volume),
            log4erl:info("~w - ItemVolume: ~w", [?MODULE, ItemVolume]),
            case compare_volume(ItemVolume, MarketOrder#market_order.item_volume) of
                greater ->
                    FillVolume = MarketOrder#market_order.item_volume,                                      
                    NewItem = Item#item { volume = ItemVolume - FillVolume},                    

                    transfer_gold(MarketOrder, BuyerId, SellerId, FillVolume), 
                    
                    log4erl:info("Remove item"),
                    %Remove fill volume from seller's item
                    item:remove(NewItem#item.id, FillVolume),                      
                    
                    log4erl:info("Create or update item"),
                    %Create new item for buyer
                    item:create(MarketOrder#market_order.city_id, 
                                BuyerId, 
                                MarketOrder#market_order.item_type, 
                                FillVolume),
                   
                    log4erl:info("Done updating seller/buyer items"), 
                    %Remove market order
                    remove_order(MarketOrder),
                    Return = {success, order_filled};
                equal ->
                    FillVolume = ItemVolume,                   

                    transfer_gold(MarketOrder, BuyerId, SellerId, FillVolume),
                    
                    %Remove seller's item
                    item:delete(Item#item.id),

                    %Create new item for buyer
                    item:create(MarketOrder#market_order.city_id, 
                                BuyerId, 
                                MarketOrder#market_order.item_type, 
                                FillVolume),

                    remove_order(MarketOrder),
                    Return = {success, order_filled};            
                less ->
                    FillVolume = ItemVolume,
                    OrderVolume = MarketOrder#market_order.item_volume - FillVolume,
                    NewMarketOrder = MarketOrder#market_order { item_volume = OrderVolume},                    
     
                    transfer_gold(MarketOrder, BuyerId, SellerId, FillVolume),

                    %Remove fill volume from seller's item 
                    item:remove(Item#item.id, FillVolume),

                    %Create new item for buyer
                    item:create(MarketOrder#market_order.city_id, 
                                BuyerId, 
                                MarketOrder#market_order.item_type, 
                                FillVolume),
                
                    update_order(NewMarketOrder),
                    Return = {success, order_filled}                    
            end;
        {not_found} ->
            log4erl:error("Insufficient item of type: ~w", [MarketOrder#market_order.item_type]),
            Return = {failure, insufficient_item}
    end,
    Return.           

transfer_gold(MarketOrder, BuyerId, SellerId, FillVolume) ->
    TotalPrice = FillVolume * MarketOrder#market_order.price,
    
    kingdom:add_gold(SellerId, TotalPrice),
    kingdom:remove_gold(BuyerId, TotalPrice).
     
compare_volume(VolumeA, VolumeB) when VolumeA > VolumeB -> greater;
compare_volume(VolumeA, VolumeB) when VolumeA =:= VolumeB -> equal;
compare_volume(VolumeA, VolumeB) when VolumeA < VolumeB -> less. 

select_volume(SelectedVolume, ItemVolume) when SelectedVolume > ItemVolume -> ItemVolume;
select_volume(SelectedVolume, ItemVolume) when SelectedVolume =< ItemVolume -> SelectedVolume.

compare_gold(PlayerGold, TotalPrice) ->
    PlayerGold >= TotalPrice.    

remove_order(MarketOrder) ->
    db:dirty_delete(market_order, MarketOrder#market_order.id).

update_order(MarketOrder) ->
    db:dirty_write(MarketOrder).

market_item_update(MarketItem) ->
    db:dirty_write(MarketItem).

market_item_remove(MarketItemId) ->
    db:dirty_delete(market_item, MarketItemId).
