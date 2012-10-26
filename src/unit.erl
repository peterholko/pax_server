%% Author: Peter
%% Created: Mar 1, 2009
%% Description: TODO: Add description to unit
-module(unit).

-behaviour(gen_server).
%%
%% Include files
%%

-include("common.hrl").
-include("schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

%%
%% Exported Functions
%%
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([create/4,
         delete/1,
         damage/3,
         transfer/2,
         add_to_queue/4,
         add_recipe/5,
         get_recipes/1,
         remove_cost/5,
         production_cost/1,
         get_unit_type/1,
         check_structure_req/2,
         is_valid_unit_type/1,
         calc_retreat_time/1,
         calc_leave_time/1,
         get_unit_ids/2,
         get_unit/1,
         get_units/1,
         highest_unit_movement/1,
         units_id/1, 
         tuple_form_items/2,
         tuple_form/1,
         get_template/1,
         get_speed/1,
         get_attack/1]).

%%
%% API Functions
%%

start() ->
    gen_server:start({global, unit_pid}, unit, [], []).

create(EntityId, Type, Size, Gear) ->
    gen_server:cast({global, unit_pid}, {'CREATE', EntityId, Type, Size, Gear}).

delete(UnitId) ->
    gen_server:cast({global, unit_pid}, {'DELETE', UnitId}).

transfer(UnitId, EntityId) ->
    gen_server:cast({global, unit_pid}, {'TRANSFER', UnitId, EntityId}).

damage(BattleId, Attacker, Defender) ->
    gen_server:call({global, unit_pid}, {'DAMAGE', BattleId, Attacker, Defender}).

get_unit(UnitId) ->
    gen_server:call({global, unit_pid}, {'GET_UNIT', UnitId}).

get_units(ArmyId) ->
    db:dirty_index_read(unit, ArmyId, #unit.entity_id).

add_to_queue(CityId, BuildingId, UnitType, UnitSize) when is_record(UnitType, unit_recipe) ->
    CurrentTime = util:get_time_seconds(),
    ContractId = counter:increment(contract),

    TargetRef = {BuildingId, ?OBJECT_BUILDING},
    Contract = #contract {id = ContractId,
                          city_id = CityId,
                          type = ?CONTRACT_UNIT,
                          target_ref = TargetRef,
                          object_type = UnitType#unit_recipe.id,
                          production = 0,
                          created_time = CurrentTime,
                          last_update = CurrentTime},

    UnitQueue = #unit_queue {contract_id = ContractId,
                             unit_type = UnitType#unit_recipe.id,
                             unit_size = UnitSize,
                             gear = UnitType#unit_recipe.gear},

    db:dirty_write(Contract),
    db:dirty_write(UnitQueue).    

add_recipe(TemplateId, PlayerId, UnitName, DefaultSize, GearList) ->
    case db:dirty_read(unit_template, TemplateId) of
        [_Template] ->
            Recipe = #unit_recipe{ id = counter:increment(unit_recipe),
                                   template_id = TemplateId,
                                   player_id = PlayerId,
                                   unit_name = UnitName,
                                   default_size = DefaultSize,
                                   gear = GearList},
            db:dirty_write(Recipe),
            Return = {success, Recipe#unit_recipe.id};
        _ ->
            Return = {error, invalid_template_id}
    end,
    Return.

get_recipes(PlayerId) ->
    Recipes = db:dirty_index_read(unit_recipe, PlayerId, #unit_recipe.player_id),

    F = fun(Recipe, RecipeList) ->
            RecipeTuple = {Recipe#unit_recipe.id,
                           Recipe#unit_recipe.template_id,
                           Recipe#unit_recipe.player_id,
                           Recipe#unit_recipe.unit_name,
                           Recipe#unit_recipe.default_size,
                           Recipe#unit_recipe.gear},
            [RecipeTuple | RecipeList]
        end,
    lists:foldl(F, [], Recipes).

remove_cost(PlayerId, CityId, UnitRecipe, UnitSize, Caste) when is_record(UnitRecipe, unit_recipe) ->
    [UnitTemplate] = db:dirty_read(unit_template, UnitRecipe#unit_recipe.template_id),
    MaterialAmount = UnitTemplate#unit_template.material_amount,
    MaterialType = UnitTemplate#unit_template.material_type,
    GearList = UnitRecipe#unit_recipe.gear,

    case check_materials(true, [], CityId, UnitSize, Caste, MaterialAmount, MaterialType) of
        {true, MaterialList} ->
            case check_gear(true, [], PlayerId, CityId, UnitSize, GearList) of
                {true, ItemList} ->
                    %Remove gear and material requirements for unit
                    remove_gear(ItemList, UnitSize),
                    remove_materials(CityId, MaterialList),
                    Result = true;
                false ->
                    Result = false
            end;
        false ->
            Result = false
    end,
    Result.

remove_gear(ItemList, Amount) ->
    F = fun(ItemId) ->
            item:remove(ItemId, Amount)
        end,          
    lists:foreach(F, ItemList).

remove_materials(CityId, MaterialList) ->
    F = fun({population, {Race, Caste}, Amount}) ->
                population:remove(CityId, Caste, Race, Amount);                        
           ({item, ItemId, Amount}) ->
                item:remove(ItemId, Amount)
        end,
    lists:foreach(F, MaterialList).

check_gear(false, _ItemList, _PlayerId, _City, _UnitSize, _GearList) ->
    false;

check_gear(true, ItemList, _PlayerId, _City, _UnitSize, []) ->
    {true, ItemList};

check_gear(true, ItemList, PlayerId, City,  UnitSize, [ItemTypeId | Rest]) ->
    case item:get_by_type({?OBJECT_CITY, City}, PlayerId, ItemTypeId) of
        {true, Item} ->
            NewItemList = [ Item#item.id | ItemList],
            Result = Item#item.volume >= UnitSize;
        false ->
            NewItemList = ItemList,
            Result = false
    end,    

    check_gear(Result, NewItemList, PlayerId, City, UnitSize, Rest).  

check_materials(false, _MaterialList, _CityId, _UnitSize, _Caste, _Amount, _Type) ->
    false;

check_materials(true, MaterialList, _City, _UnitSize, _Caste, [], []) ->
    {true, MaterialList};

check_materials(true, MaterialList, City, UnitSize, Caste, [Amount | RestAmount], [CategoryId | RestCategory]) ->
    TotalAmount = Amount * UnitSize,
    case item_category:is_population(CategoryId) of
        {true, Race} ->
            case population:check_population(City, Caste, Race, TotalAmount) of
                true ->
                    NewMaterialList = [{population, {Race, Caste}, TotalAmount} | MaterialList],
                    Result = true;
                false ->
                    NewMaterialList = MaterialList,
                    Result = false
            end;
        false ->
            case item_category:find_available_item(City, CategoryId, TotalAmount) of
                {true, ItemId} ->
                    NewMaterialList = [{item, ItemId, TotalAmount} | MaterialList],
                    Result = true;
                false ->
                    NewMaterialList = MaterialList,
                    Result = false
            end
    end,
    check_materials(Result, NewMaterialList, City, UnitSize, Caste, RestAmount, RestCategory).

production_cost(UnitType) when is_record(UnitType, unit_recipe) ->
    [UnitTemplate] = db:dirty_read(unit_template, UnitType#unit_recipe.template_id),
    UnitTemplate#unit_template.production_cost;

production_cost(_UnitType) ->
    ?ERROR("Invalid unit_type"),
    erlang:error("Invalid unit_type").

get_unit_type(UnitTypeId) ->
    case db:dirty_read(unit_recipe, UnitTypeId) of
        [UnitRecipe] ->
            Result = {true, UnitRecipe};
        _ ->
            Result = false
    end,
    Result.

check_structure_req(Building, UnitRecipe) when is_record(Building, building),
                                               is_record(UnitRecipe, unit_recipe) ->
    case building:get_building_type(Building) of
        {true, BuildingType} ->
            [UnitTemplate] = db:dirty_read(unit_template, UnitRecipe#unit_recipe.template_id),
            Result = BuildingType#building_type.id == UnitTemplate#unit_template.building_req;
        false ->
            Result = false
    end,
    Result;

check_structure_req(_Building, _UnitRecipe) ->
    false.

is_valid_unit_type(UnitType) ->
    case db:dirty_read(unit_type, UnitType) of
        [UnitType] ->
            Result = true;
        _ ->
            Result = false
    end,
    Result.

calc_retreat_time(_ArmyId) ->
    10.

calc_leave_time(_ArmyID) ->
    5.

get_unit_ids([], UnitIds) ->
    UnitIds;

get_unit_ids(ListUnits, UnitIds) ->
    [Unit | Rest] = ListUnits,
    NewUnitIds = [Unit#unit.id | UnitIds],
    get_unit_ids(Rest, NewUnitIds).

highest_unit_movement(ArmyId) ->
    Units = db:dirty_index_read(unit, ArmyId, #unit.entity_id), 
    F = fun(Unit, UnitList) ->
            [UnitTemplate] = db:dirty_read(unit_template, Unit#unit.template_id),
            UnitSpeed = UnitTemplate#unit_template.movement,
            [UnitSpeed | UnitList]
        end,

    UnitSpeeds = lists:foldl(F, [], Units),
    lists:max(UnitSpeeds).

units_id([]) ->
    [];

units_id(Units) ->
    F = fun(Unit, UnitList) ->
                UnitId = Unit#unit.id,
                [UnitId | UnitList]
        end,
    
    lists:foldl(F, [], Units).

tuple_form_items(PlayerId, EntityId) ->
    Units = db:dirty_index_read(unit, EntityId, #unit.entity_id),

    F = fun(Unit, UnitList) ->
            Items = item:get_by_entity({?OBJECT_UNIT, Unit#unit.id}, PlayerId),
            ItemsTuple = item:tuple_form(Items),

            UnitTuple = {Unit#unit.id, 
                         Unit#unit.recipe_id, 
                         Unit#unit.size,
                         Unit#unit.name,
                         Unit#unit.gear,
                         ItemsTuple},
            [UnitTuple | UnitList]
        end,
    
    lists:foldl(F, [], Units).

tuple_form(EntityId) ->
    Units = db:dirty_index_read(unit, EntityId, #unit.entity_id),
   
    F = fun(Unit, UnitList) ->                        
            UnitName = get_name(Unit),
            UnitTuple = {Unit#unit.id,
                         UnitName,
                         Unit#unit.template_id,
                         Unit#unit.size},
            [UnitTuple | UnitList]
    end,
    
    lists:foldl(F, [], Units).

get_name(Unit) when is_record(Unit, unit) ->
    case db:dirty_read(unit_recipe, Unit#unit.recipe_id) of
        [Recipe] ->
            Name = Recipe#unit_recipe.unit_name;
        _ ->
            [Template] = db:dirty_read(unit_template, Unit#unit.template_id),
            Name = Template#unit_template.name
    end,
    Name.

get_template(Unit) when is_record(Unit, unit) ->
    [UnitTemplate] = db:dirty_read(unit_template, Unit#unit.template_id),
    UnitTemplate.

get_speed(Template) when is_record(Template, unit_template) ->
    Template#unit_template.speed.

get_attack(Template) when is_record(Template, unit_template) ->
    Template#unit_template.atk.

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    {ok, []}.

handle_cast({'CREATE', EntityId, Type, Size, Gear}, Data) ->
    create_unit(EntityId, Type, Size, Gear),
    {noreply, Data};

handle_cast({'DELETE', _UnitId}, Data) ->
    {noreply, Data};

handle_cast({'TRANSFER', UnitId, EntityId}, Data) ->
    transfer_unit(UnitId, EntityId),
    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({'DAMAGE', BattleId, Attacker, Defender}, _From, Data) ->
    Result = process_damage(BattleId, Attacker, Defender),
    {reply, Result, Data};

handle_call({'GET_UNIT', UnitId}, _From, Data) ->
    case db:dirty_read(unit, UnitId) of
        [Unit] ->
            Result = Unit;
        _ ->
            Result = false
    end,
    {reply, Result, Data};

handle_call({'GET_UNITS', EntityId}, _From, Data) ->
    Units = db:dirty_index_read(unit, EntityId, #unit.entity_id),
    {reply, Units, Data};

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

create_unit(EntityId, Type, Size, Gear) ->
    new_unit(EntityId, Type, Size, Gear).

transfer_unit(UnitId, EntityId) ->
    [Unit] = db:dirty_read(unit, UnitId),
    NewUnit = Unit#unit{entity_id = EntityId},
    db:dirty_write(NewUnit).
    
destroy_unit(BattleId, EntityId, UnitId) ->
    PlayerId = entity:player_id(EntityId),
    Items = item:get_by_entity({?OBJECT_UNIT, UnitId}, PlayerId),


    F = fun(Item) ->
            % Remove 75% of the item volume
            NewVolume = util:ceiling(Item#item.volume * 0.25),
            item:set(Item#item.id, NewVolume),
            
            % No player id
            item:transfer(Item#item.id, {?OBJECT_BATTLE, BattleId}, -1)
        end,

    lists:foreach(F, Items),

    db:dirty_delete(unit, UnitId).

new_unit(EntityId, RecipeId, Size, Gear) ->
    UnitId = counter:increment(unit) + 1000,
    PlayerId = entity:player_id(EntityId),

    [Recipe] = db:dirty_read(unit_recipe, RecipeId),
    [Template] = db:dirty_read(unit_template, Recipe#unit_recipe.template_id),

    %create the unit's gear
    ItemIdList = create_gear(UnitId, PlayerId, Size, Gear),

    Unit = #unit {id = UnitId,
                  entity_id = EntityId,
                  recipe_id = RecipeId,
                  template_id = Template#unit_template.id,
                  size = Size,
                  current_hp = Template#unit_template.hp,
                  name = Recipe#unit_recipe.unit_name,
                  gear = ItemIdList},

    db:dirty_write(Unit). 

create_gear(UnitId, PlayerId, Amount, GearList) ->
    
    F = fun(ItemTypeId, ItemIdList) ->
            ItemId = item:create_get_id({?OBJECT_UNIT, UnitId}, PlayerId, ItemTypeId, Amount),
            [ItemId | ItemIdList]
        end,

    lists:foldl(F, [], GearList).

process_damage(BattleId, Attacker, Defender) ->
    AtkTemplate = unit:get_template(Attacker),
    DefTemplate = unit:get_template(Defender),

    AtkSize = Attacker#unit.size,
    DefSize = Defender#unit.size,

    DefTempHp = DefTemplate#unit_template.hp,
    DefCurrentHp =  Defender#unit.current_hp,

    Atk = AtkTemplate#unit_template.atk,
    %Acc = AtkTemplate#unit_template.acc,

    Def = DefTemplate#unit_template.def,
    %Eva = DefTemplate#unit_template.eva,

    %Calculate damage per unit
    DamagePerUnit = (Atk * 1) - (Def * 0.5),

    %Calculate total damage
    TotalDamage = DamagePerUnit * AtkSize,
    
    %Calculate total defender hp
    DefTotalHp = DefTempHp * (DefSize - 1) + DefCurrentHp,

    case TotalDamage >= DefTotalHp of
        true ->
            ?INFO("Unit destroyed: ", Defender#unit.id),
            destroy_unit(BattleId, Defender#unit.entity_id, Defender#unit.id),

            case get_num_units(Defender#unit.entity_id) > 0 of
                true ->
                    Result = {unit_destroyed, TotalDamage};
                false ->
                    Result = {army_destroyed, TotalDamage}
            end;
        false ->     
            %Calculate number of defenders killed
            NumKilled = TotalDamage div DefTempHp,
            ?INFO("Units killed: ", NumKilled),

            %Calculate remaining damage
            RemainingDamage = TotalDamage - NumKilled * DefTempHp,

            %Set new defender size and current hp
            NewDefSize = DefSize#unit.size - NumKilled,
            NewDefCurrentHp = DefCurrentHp - RemainingDamage,

            %Set new unit variables
            NewDefender = Defender#unit { size = NewDefSize,
                                          current_hp = NewDefCurrentHp},

            db:dirty_write(NewDefender),
            Result = {unit_damaged, TotalDamage}
    end,
    Result.

get_num_units(EntityId) ->
    Units = db:dirty_index_read(unit, EntityId, #unit.entity_id),
    length(Units).
