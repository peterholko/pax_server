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
-export([create/4, create_get_id/4, delete/1, set/2, transfer/3, get_by_type/3, get_by_entity/2]).
-export([get_lumber/2, get_stone/2]).
-export([check_amount/4]).
-export([add/2, remove/2, get_volume/1]).
-export([complete/4, add_recipe/5, get_recipes/1, get_recipe/2]).
-export([tuple_form/1, add_to_queue/6]).
-export([get_item_type/1, production_cost/1]).
-export([check_req/2, is_category/2, is_resource/1, remove_cost/4]).
-record(module_data, {}).
%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, item_pid}, item, [], []).

create({OwnerType, OwnerId}, PlayerId, Type, Volume) ->
    gen_server:cast({global, item_pid}, {'CREATE_ITEM', {OwnerType, OwnerId}, PlayerId, Type, Volume}).

create_get_id({OwnerType, OwnerId}, PlayerId, Type, Volume) ->
    gen_server:call({global, item_pid}, {'CREATE_ITEM_GET_ID', {OwnerType, OwnerId}, PlayerId, Type, Volume}).

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

transfer(ItemId, {OwnerType, OwnerId}, PlayerId) ->
    gen_server:cast({global, item_pid}, {'TRANSFER_ITEM', ItemId, {OwnerType, OwnerId}, PlayerId}).

get_volume(ItemId) ->
    gen_server:call({global, item_pid}, {'VOLUME', ItemId}).

get_by_type({OwnerType, OwnerId}, PlayerId, Type) ->
    gen_server:call({global, item_pid}, {'GET_ITEM_BY_TYPE', {OwnerType, OwnerId}, PlayerId, Type}).

get_lumber({OwnerType, OwnerId}, PlayerId) ->
    gen_server:call({global, item_pid}, {'GET_LUMBER', {OwnerType, OwnerId}, PlayerId}).

get_stone({OwnerType, OwnerId}, PlayerId) ->
    gen_server:call({global, item_pid}, {'GET_STONE', {OwnerType, OwnerId}, PlayerId}).

get_by_entity({OwnerType, OwnerId}, PlayerId) ->
    gen_server:call({global, item_pid}, {'GET_ITEM_BY_ENTITY', {OwnerType, OwnerId}, PlayerId}).

tuple_form(Items) ->
    F = fun(Item, ItemList) ->
        {{OwnerType, OwnerId}, PlayerId} = Item#item.ref,
        ItemTuple = {Item#item.id,
                     OwnerType,
                     OwnerId,
                     PlayerId,
                     Item#item.type,
                     Item#item.template_id,
                     Item#item.volume},
        [ItemTuple | ItemList]
    end,
    lists:foldl(F, [], Items).

complete(CityId, PlayerId, ItemType, ItemAmount) when is_record(ItemType, item_base) ->
    case ItemType#item_base.produces of
        [] ->
            item:create({?OBJECT_CITY, CityId}, PlayerId, ItemType#item_base.type_id, ItemAmount);
        Produces ->
            F = fun(ItemTypeId) ->
                    item:create({?OBJECT_CITY, CityId},
                                PlayerId,
                                ItemTypeId,
                                ItemAmount)
                end,
            lists:foreach(F, Produces)
    end;

complete(CityId, PlayerId, ItemType, ItemAmount) when is_record(ItemType, item_recipe) ->
    item:create({?OBJECT_CITY, CityId}, PlayerId, ItemType#item_recipe.type_id, ItemAmount);

complete(_, _, _, _) ->
    ?INFO("Invalid item:complete match").

check_amount({OwnerType, OwnerId}, PlayerId, Type, Amount) ->
    case get_by_type({OwnerType, OwnerId}, PlayerId, Type) of
        {true, Item} ->
            Result = Item#item.volume >= Amount;
        false ->
            Result = false
    end,
    Result.

add_recipe(TemplateId, PlayerId, ItemName, FlavourText, RecipeTypeList) ->
    case db:dirty_read(item_template, TemplateId) of
        [Template] ->
            RecipeCategoryList = type_to_category(RecipeTypeList),
            TemplateCategoryList = Template#item_template.material_category,
            if 
                RecipeCategoryList =:= TemplateCategoryList ->
                    Recipe = #item_recipe { type_id = counter:increment(item_recipe) + ?ITEM_RECIPE_ID_OFFSET,
                                            template_id = TemplateId,
                                            player_id = PlayerId,
                                            item_name = ItemName,
                                            flavour_text = FlavourText,
                                            material_amount = Template#item_template.material_amount,
                                            material_type = RecipeTypeList},
                    db:dirty_write(Recipe),
                    Return = {success, Recipe#item_recipe.type_id};
                true ->
                    Return = {error, invalid_recipe}
            end;
        _ ->
            Return = {error, invalid_template_id}
    end,
    Return.

get_recipe(TypeId, PlayerId) ->
    case db:dirty_read(item_recipe, TypeId) of
        [ItemRecipe] ->
            if
                PlayerId == ItemRecipe#item_recipe.player_id ->
                    Return = {true, ItemRecipe};
                true ->
                    Return = false
            end;
        _ ->
            Return = false 
    end,
    Return.   

type_to_category(TypeList) ->
    F = fun(TypeId, CategoryList) ->
            [ItemBase] = db:dirty_read(item_base, TypeId),
            [ItemCategory] = db:dirty_index_read(item_category, ItemBase#item_base.category, #item_category.display_name),
            [ItemCategory#item_category.id | CategoryList]
        end,
   
    lists:foldl(F, [], TypeList).

get_recipes(PlayerId) ->
    Recipes = db:dirty_index_read(item_recipe, PlayerId, #item_recipe.player_id),
    
    F = fun(Recipe, RecipeList) ->
            RecipeTuple = {Recipe#item_recipe.type_id,
                           Recipe#item_recipe.template_id,        
                           Recipe#item_recipe.player_id,
                           Recipe#item_recipe.item_name,
                           Recipe#item_recipe.flavour_text,
                           Recipe#item_recipe.material_amount,
                           Recipe#item_recipe.material_type},
            [RecipeTuple | RecipeList]        
        end,
    lists:foldl(F, [], Recipes).

get_item_type(ItemTypeId) ->
    case db:dirty_read(item_base, ItemTypeId) of
        [ItemBaseType] ->
            Result = {true, ItemBaseType};
        _ ->
            case db:dirty_read(item_recipe, ItemTypeId) of
                [ItemRecipe] ->
                    Result = {true, ItemRecipe};
                _ ->
                    Result = false
            end
    end,
    Result.

is_category(ItemType, Category) when is_record(ItemType, item_base) ->
    ItemType#item_base.category =:= Category;

is_category(ItemType, Category) when is_record(ItemType, item_recipe) ->
    [ItemTemplate] = db:dirty_read(item_template, ItemType#item_recipe.template_id),
    ItemTemplate#item_template.category =:= Category;

is_category(_ItemType, _Category) ->
    false.

is_resource(ItemType) when is_record(ItemType, item_base) ->
    case db:dirty_read(resource_type, ItemType#item_base.material_type) of
        [_ResourceType] ->
            true;
        _ ->
            false
    end;

is_resource(_ItemType) ->
    false.

production_cost(ItemType) when is_record(ItemType, item_base) ->
    ItemType#item_base.production_cost;

production_cost(ItemType) when is_record(ItemType, item_recipe) ->
    [ItemTemplate] = db:dirty_read(item_template, ItemType#item_recipe.template_id),
    ItemTemplate#item_template.production_cost;

production_cost(_ItemType) ->
    ?ERROR("Invalid item_type"),
    erlang:error("Invalid item_type").

check_req(Improvement, ItemType) when is_record(Improvement, improvement),
                                      is_record(ItemType, item_base) ->
    case improvement:get_improvement_type(Improvement#improvement.type) of
        {true, ImprovementType} ->
            Result = ImprovementType#improvement_type.id == ItemType#item_base.improvement_req;
        false ->
            Result = false
    end,
    Result;

check_req(Building, ItemType) when is_record(Building, building),
                                   is_record(ItemType, item_base) ->
    case building:get_building_type(Building) of
        {true, BuildingType} ->
            Result = BuildingType#building_type.id == ItemType#item_base.building_req;
        false ->
            Result = false
    end,
    Result;

check_req(Building, ItemRecipe) when is_record(Building, building),
                                   is_record(ItemRecipe, item_recipe) ->
    case building:get_building_type(Building) of
        {true, BuildingType} ->
            [ItemTemplate] = db:dirty_read(item_template, ItemRecipe#item_recipe.template_id),
            Result = BuildingType#building_type.id == ItemTemplate#item_template.building_req;
        false ->
            Result = false
    end,
    Result;

check_req(_Type, _Object) ->
    false.

remove_cost({OwnerType, OwnerId}, PlayerId, ItemType, ItemAmount) when is_record(ItemType, item_base) ->
    io:fwrite("City: ~w Player: ~w ItemType: ~w ItemAmount: ~w~n", [{OwnerType, OwnerId}, PlayerId, ItemType, ItemAmount]),
    MaterialType = ItemType#item_base.material_type,
    MaterialAmount = ItemType#item_base.material_amount,
    TotalAmount = ItemAmount * MaterialAmount,

    case item:get_by_type({OwnerType, OwnerId}, PlayerId, MaterialType) of
        {true, Item} ->
            if
                TotalAmount =< Item#item.volume ->
                    item:remove(Item#item.id, TotalAmount),
                    Result = true;
                true ->
                    Result = false
            end;
         false ->
            Result = false
    end,
    Result;

remove_cost({OwnerType, OwnerId}, PlayerId, ItemType, ItemAmount) when is_record(ItemType, item_recipe) ->
    io:fwrite("City: ~w Player: ~w ItemType: ~w ItemAmount: ~w~n", [{OwnerType, OwnerId}, PlayerId, ItemType, ItemAmount]),
    [ItemTemplate] = db:dirty_read(item_template, ItemType#item_recipe.template_id),
    MaterialAmount = ItemTemplate#item_template.material_amount,
    MaterialType = ItemType#item_recipe.material_type,

    case check_materials(true, [], {OwnerType, OwnerId}, PlayerId, ItemAmount, MaterialAmount, MaterialType) of
        {true, ItemList} ->
            F = fun({Item, Amount}) ->
                    item:remove(Item#item.id, Amount)
                end,

            lists:foreach(F, ItemList),
            Result = true;
        false ->
            Result = false  
    end,
    Result.          
          
check_materials(false, _ItemList, _OwnerRef, _PlayerId, _ItemAmount, _MaterialAmounts, _MaterialTypes) ->
    false;

check_materials(true, ItemList, _OwnerRef, _PlayerId, _ItemAmount, [], []) ->
    {true, ItemList};

check_materials(true, ItemList, {OwnerType, OwnerId}, PlayerId, ItemAmount, [MaterialAmount | RestAmount], 
                [MaterialType | RestType]) ->
    
    TotalAmount = ItemAmount * MaterialAmount,
    io:fwrite("{OwnerType, OwnerId}: ~w PlayerId: ~w MaterialType: ~w~n", [{OwnerType, OwnerId}, PlayerId, MaterialType]),
    case item:get_by_type({OwnerType, OwnerId}, PlayerId, MaterialType) of
        {true, Item} ->
            if
                TotalAmount =< Item#item.volume ->
                    NewItemList = [{Item, TotalAmount} | ItemList],
                    Result = true;
                true ->
                    NewItemList = ItemList,
                    Result = false
            end;
        false ->
            NewItemList = ItemList,
            Result = false
    end,
    check_materials(Result, NewItemList, {OwnerType, OwnerId}, PlayerId, ItemAmount, RestAmount, RestType).    

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

handle_cast({'CREATE_ITEM', OwnerRef, PlayerId, Type, Volume},Data) ->  
    log4erl:info("{~w} CREATE_ITEM", [?MODULE]),  
    create_item(OwnerRef, PlayerId, Type, Volume),
    {noreply, Data};

handle_cast({'DELETE_ITEM', ItemId}, Data) ->
    delete_item(ItemId),
    {noreply, Data};

handle_cast({'SET_ITEM', ItemId, Volume}, Data) ->
    set_item(ItemId, Volume),
    {noreply, Data};

handle_cast({'UPDATE_ITEM', ItemId, Volume}, Data) ->
    update_item(ItemId, Volume),
    {noreply,  Data};

handle_cast({'TRANSFER_ITEM', ItemId, OwnerRef, PlayerId}, Data) ->
    transfer_item(ItemId, OwnerRef, PlayerId),
    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({'CREATE_ITEM_GET_ID', OwnerRef, PlayerId, Type, Volume}, _From, Data) ->
    log4erl:info("{~w} CREATE_ITEM_GET_ID", [?MODULE]),
    ItemId = create_item(OwnerRef, PlayerId, Type, Volume),
    {reply, ItemId, Data};

handle_call({'GET_VOLUME', ItemId}, _From, Data) ->
    Item = get_item(ItemId),
    {reply, Item#item.volume, Data};

handle_call({'GET_ITEM_BY_TYPE', OwnerRef, PlayerId, Type}, _From, Data) ->
    Item = get_item_by_type(OwnerRef, PlayerId, Type),
    {reply, Item, Data};

handle_call({'GET_ITEM_BY_ENTITY', OwnerRef, PlayerId}, _From, Data) ->
    Item = get_item_by_entity(OwnerRef, PlayerId),
    {reply, Item, Data};

handle_call({'GET_LUMBER', OwnerRef, PlayerId}, _From, Data) ->
    Lumber = get_all_lumber(OwnerRef, PlayerId),
    {reply, Lumber, Data};

handle_call({'GET_STONE', OwnerRef, PlayerId}, _From, Data) ->
    Stone = get_all_stone(OwnerRef, PlayerId),
    {reply, Stone, Data};

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
create_item(OwnerRef, PlayerId, Type, Volume) ->
    case db:dirty_read(item_type_ref, {OwnerRef, PlayerId, Type}) of
        [] ->
            ItemId = new_item(OwnerRef, PlayerId, Type, Volume);
        [ItemTypeRef | _Rest] ->
            ItemId = ItemTypeRef#item_type_ref.item_id,
            update_item(ItemId, Volume)
    end,
    
    %Return ItemId
    ItemId.

delete_item(ItemId) ->
    [_Item] = db:dirty_read(item, ItemId),
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

transfer_item(ItemId, TargetOwnerRef, TargetPlayerId) ->
    ?INFO("transfer_item"),
    [Item] = db:dirty_read(item, ItemId),
    {OwnerRef, PlayerId} = Item#item.ref,
    db:dirty_delete(item_type_ref, {OwnerRef, PlayerId, Item#item.type}),

    ItemRef = {TargetOwnerRef, TargetPlayerId},
    NewItem = Item#item { ref = ItemRef},

    ItemTypeRef = #item_type_ref {owner_type_ref = {TargetOwnerRef, TargetPlayerId, Item#item.type},                                
                                  item_id = ItemId},
    db:dirty_write(NewItem),
    db:dirty_write(ItemTypeRef).

set_item(ItemId, Volume) ->
    [Item] = db:dirty_read(item, ItemId),
    NewItem = Item#item {volume = Volume},
    db:dirty_write(NewItem).

new_item(OwnerRef, PlayerId, Type, Volume) ->
    ItemId = counter:increment(item) + 1000,
    TemplateId = get_template_id(Type), 
    Item = #item {id = ItemId,
                  ref = {OwnerRef, PlayerId},
                  type = Type,
                  template_id = TemplateId,
                  volume = Volume},
    ItemTypeRef = #item_type_ref {owner_type_ref = {OwnerRef, PlayerId, Type},                               
                                  item_id = ItemId},

    db:dirty_write(Item),
    db:dirty_write(ItemTypeRef),

    %Return Item ID
    ItemId.

get_item(ItemId) ->
    db:dirty_read(item, ItemId).

get_item_by_type(OwnerRef, PlayerId, Type) ->
    case db:dirty_read(item_type_ref, {OwnerRef, PlayerId, Type}) of
        [ItemTypeRef] ->
            [Item] = db:dirty_read(item, ItemTypeRef#item_type_ref.item_id),
            Result = {true, Item};
        _ ->
            Result = false
    end,
    Result.

get_item_by_entity(OwnerRef, PlayerId) ->
    db:dirty_index_read(item, {OwnerRef, PlayerId}, #item.ref).    

get_item_type_list(OwnerRef, PlayerId, Type) ->
    case db:dirty_read(item_type_ref, {OwnerRef, PlayerId, Type}) of
        [ItemTypeRef] ->
            Result = db:dirty_read(item, ItemTypeRef#item_type_ref.item_id);
        _ ->
            Result = []
    end,
    Result.

get_template_id(ItemType) ->
    case db:dirty_read(item_recipe, ItemType) of 
        [ItemRecipe] ->
            TemplateId = ItemRecipe#item_recipe.template_id;
        _ ->
            TemplateId = -1
    end,
    TemplateId.

get_all_lumber(OwnerRef, PlayerId) ->
    Lumber1 = get_item_type_list(OwnerRef, PlayerId, ?ITEM_LUMBER1),
    Lumber1.

get_all_stone(OwnerRef, PlayerId) ->
    Stone1 = get_item_type_list(OwnerRef, PlayerId, ?ITEM_STONE1),
    Stone2 = get_item_type_list(OwnerRef, PlayerId, ?ITEM_STONE2),
    Stone1 ++ Stone2. 
