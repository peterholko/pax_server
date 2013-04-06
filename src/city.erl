% Author: Peter
%% Created: Feb 4, 2009
%% Description: TODO: Add description to city
-module(city).
-behaviour(gen_server).

%%
%% Include files
%%

-include("game.hrl").
-include("common.hrl").
-include("schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

%%
%% Exported Functions
%%
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-export([start/2, stop/1]).
-export([queue_unit/6,
         queue_building/2, 
         queue_improvement/4,
         craft_item/5,
         update_tax/2,
         add_claim/4, 
         remove_claim/2, 
         assign_task/6,
         remove_task/2,
         get_coords/1,
         form_army/2]).

-record(module_data, {city,                       
                      units_queue,
                      improvements = [],
                      inventory,
                      player_id, 
                      self,
                      visible = [],
                      observed_by = []}).
%%
%% API Functions
%%
start(CityId, PlayerId) ->
    case db:read(city, CityId) of 
        [City] ->
            gen_server:start({global, {city, CityId}}, city, [City, PlayerId], []);
        Any ->
            {error, Any}
    end.

init([City, PlayerId]) 
  when is_tuple(City),
       is_integer(PlayerId) ->
    process_flag(trap_exit, true),
    
    %Inventory setup    
    Inventory = dict:new(),

    {ok, #module_data{city = City, 
                      inventory = Inventory, 
                      player_id = PlayerId, 
                      self = self() }}.

terminate(_Reason, _) ->
    ok.

stop(ProcessId) 
  when is_pid(ProcessId) ->
    gen_server:cast(ProcessId, stop).

queue_unit(CityId, BuildingId, UnitType, UnitSize, Caste, Race) ->
    gen_server:call(global:whereis_name({city, CityId}), {'QUEUE_UNIT', BuildingId, UnitType, UnitSize, Caste, Race}).

queue_building(CityId, BuildingType) ->
    gen_server:call(global:whereis_name({city, CityId}), {'QUEUE_BUILDING', BuildingType}).

queue_improvement(CityId, X, Y, ImprovementType) ->
    gen_server:call(global:whereis_name({city, CityId}), {'QUEUE_IMPROVEMENT', X, Y, ImprovementType}).

craft_item(CityId, SourceId, SourceType, ItemType, Amount) ->
    gen_server:call(global:whereis_name({city, CityId}), {'CRAFT_ITEM', SourceId, SourceType, ItemType, Amount}).

update_tax(CityId, Taxes) ->
    gen_server:call(global:whereis_name({city, CityId}), {'UPDATE_TAX', Taxes}).

add_claim(CityId, ArmyId, X, Y) ->
    gen_server:call(global:whereis_name({city, CityId}), {'ADD_CLAIM', ArmyId, X, Y}).

remove_claim(CityId, ClaimId) ->
    gen_server:call(global:whereis_name({city, CityId}), {'REMOVE_CLAIM', ClaimId}).

assign_task(CityId, Caste, Race, Amount, TaskId, TaskType) ->
    gen_server:call(global:whereis_name({city, CityId}), {'ASSIGN_TASK', Caste, Race, Amount, TaskId, TaskType}).

remove_task(CityId, AssignmentId) ->
    gen_server:call(global:whereis_name({city, CityId}), {'REMOVE_TASK', AssignmentId}).

get_coords(CityId) ->
    gen_server:call(global:whereis_name({city, CityId}), {'GET_COORDS'}).

form_army(CityId, ArmyName) ->
    gen_server:call(global:whereis_name({city, CityId}), {'FORM_ARMY', ArmyName}).

%%
%% OTP handlers
%%

handle_cast({'ADD_VISIBLE', _CityId, EntityId, EntityPid}, Data) ->
    VisibleList = Data#module_data.visible,
    NewVisibleList = [{EntityId, EntityPid} | VisibleList],
    NewData = Data#module_data { visible = NewVisibleList },
    
    update_perception(Data#module_data.player_id),

    {noreply, NewData};

handle_cast({'REMOVE_VISIBLE', _CityId, EntityId, EntityPid}, Data) ->
    VisibleList = Data#module_data.visible,
    NewVisibleList = lists:delete({EntityId, EntityPid}, VisibleList),
    NewData = Data#module_data { visible = NewVisibleList },

    update_perception(Data#module_data.player_id),
    
    {noreply, NewData};

handle_cast({'ADD_OBSERVED_BY', _CityId, EntityId, EntityPid}, Data) ->
    ObservedByList = Data#module_data.observed_by,
    NewObservedByList = [{EntityId, EntityPid} | ObservedByList],
    NewData = Data#module_data { observed_by = NewObservedByList },
    
    {noreply, NewData};

handle_cast({'REMOVE_OBSERVED_BY', _CityId, EntityId, EntityPid}, Data) ->
    ObservedByList = Data#module_data.observed_by,
    NewObservedByList = lists:delete({EntityId, EntityPid}, ObservedByList),
    NewData = Data#module_data { observed_by = NewObservedByList },
    
    {noreply, NewData};

handle_cast({'PROCESS_EVENT',_EventTick, _EventData, EventType}, Data) ->   
    City = Data#module_data.city,

    case EventType of
        ?EVENT_GROWTH ->
            ?INFO("Processing growth event"),
            income(City),
            growth(City#city.id, City#city.player_id) 
    end,      
    
    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({'ADD_CLAIM', ArmyId, X, Y}, _From, Data) ->
    City = Data#module_data.city,
    TileIndex = map:convert_coords(X, Y),
       
    ValidX = abs(City#city.x - X) =< 6,
    ValidY = abs(City#city.y - Y) =< 6,
    Exists = length(db:index_read(claim, TileIndex, #claim.tile_index)) > 0,
    MaxReached = length(db:index_read(claim, City#city.id, #claim.city_id)) > 3,

    %log4erl:debug("ValidX: ~w ValidY: ~w Exists: ~w MaxReached: ~w~n", [ValidX, ValidY, Exists, MaxReached]), 

    case claim:is_valid(ValidX, ValidY, Exists, MaxReached) of
        true ->
            log4erl:info("{~w} Add claim successful.", [?MODULE]),         
            ClaimId = counter:increment(claim),

            NewClaimRecord = #claim {id = ClaimId,
                                     tile_index = TileIndex,
                                     city_id = City#city.id,
                                     army_id = ArmyId, 
                                     state = ?STATE_IN_PROGRESS,
                                     created_time = util:get_time_seconds()},
            db:write(NewClaimRecord),
            Result = {success, ClaimId};
        false ->
            Result = {failure, "Check Failed"}
    end,

    {reply, Result, Data};

handle_call({'REMOVE_CLAIM', ClaimId}, _From, Data) ->
    City = Data#module_data.city,
   
    case db:read(claim, ClaimId) of
        [Claim] ->
            case Claim#claim.city_id =:= City#city.id of
                true ->
                    db:delete(Claim, ClaimId),
                    Result = {success, ClaimId};
                false ->
                    Result = {failure, "Claim does not belong to city"}
            end;
        _ ->
            Result = {failure, "Claim does not exist"}
    end,

    {reply, Result, Data};

handle_call({'FORM_ARMY', ArmyName}, _From, Data) ->
    City = Data#module_data.city,

    _ArmyId = army_manager:create(City#city.player_id, City#city.x, City#city.y, ArmyName),
    Result = {city, formed_army},

    {reply, Result, Data};

handle_call({'QUEUE_UNIT', BuildingId, UnitTypeId, UnitSize, Caste, _Race}, _From, Data) ->   
    City = Data#module_data.city,
    CityId = City#city.id,
    PlayerId = City#city.player_id,

    case structure:is_available(BuildingId, ?OBJECT_BUILDING) of
        {true, Structure} ->
            case unit:get_unit_type(UnitTypeId) of
                {true, UnitType} ->
                    case unit:check_structure_req(Structure, UnitType) of
                        true ->
                            case unit:remove_cost(PlayerId, CityId, UnitType, UnitSize, Caste) of
                                true ->
                                    unit:add_to_queue(CityId, BuildingId, UnitType, UnitSize),
                                    Result = {city, queued_unit};
                                false ->
                                    Result = {city, insufficient_materials}
                            end;
                        false ->
                            Result = {city, invalid_structure_req}
                    end;
                false ->
                    Result = {city, invalid_type}
            end;
        false ->
            Result = {city, no_structure}
    end, 
    {reply, Result, Data};

handle_call({'QUEUE_BUILDING', BuildingType}, _From, Data) ->
    ?INFO("Queue Building Type", BuildingType),
    City = Data#module_data.city,      
    PlayerId = City#city.player_id,
  
    IsValid = building:is_valid(PlayerId =:= City#city.player_id,
                                building:check_type(BuildingType)),
    case IsValid of
        true ->                        
            case structure:remove_cost(building, City#city.id, PlayerId, BuildingType) of
                true ->
                    building:add_to_queue(City#city.id, BuildingType),
                    Result = {city, queued_building};
                false ->
                    Result = {city, insuffucient_materials}
            end;
        {false, ErrorType} ->
            Result = {city, ErrorType}
    end,

    {reply, Result, Data}; 

handle_call({'QUEUE_IMPROVEMENT', X, Y, ImprovementType}, _From, Data) ->
    City = Data#module_data.city,
    PlayerId = Data#module_data.player_id,

    TileIndex = map:convert_coords(X,Y),

    CheckClaimStatus = claim:check_status(TileIndex), 
    CheckEmpty = improvement:empty(X,Y), 
    CheckType = improvement:check_type(ImprovementType),

    IsValid = improvement:is_valid(CheckClaimStatus,
                                   CheckEmpty,
                                   CheckType),

    case IsValid of
        true ->
            case structure:remove_cost(improvement, City#city.id, PlayerId, ImprovementType) of
                true ->
                    improvement:queue(X, Y, PlayerId, City#city.id, ImprovementType),
                    Result = {city, queued_improvement};
                false ->
                    Result = {city, insufficient_materials}
            end;
        {false, ErrorType} ->
            log4erl:info("{~w} Queue Improvement failed: ~w", [?MODULE, ErrorType]),
            Result = {city, ErrorType}
            
    end,

    {reply, Result, Data};

handle_call({'CRAFT_ITEM', SourceId, SourceType, ItemTypeId, ItemAmount}, _From, Data) ->
    City = Data#module_data.city,
    PlayerId = City#city.player_id,
    CityId = City#city.id,

    case structure:is_available(SourceId, SourceType) of
        {true, Structure} ->
            case item:get_item_type(ItemTypeId) of
                {true, ItemType} ->
                    case item:check_req(Structure, ItemType) of
                        true -> 
                            case item:is_resource(ItemType) of
                                true ->
                                    item:add_to_queue(PlayerId,
                                                      CityId,
                                                      ?CONTRACT_HARVEST,
                                                      {SourceId, ?OBJECT_IMPROVEMENT},
                                                      ItemTypeId,
                                                      ItemAmount),
                                    Result = {city, queued_crafted};
                                false ->
                                    case item:remove_cost({?OBJECT_CITY, CityId}, PlayerId, ItemType, ItemAmount) of
                                        true ->
                                            item:add_to_queue(PlayerId,
                                                              CityId,
                                                              ?CONTRACT_ITEM,
                                                              {SourceId, ?OBJECT_BUILDING},
                                                              ItemTypeId,
                                                              ItemAmount),
                                            Result = {city, queued_crafted};
                                        false ->
                                            Result = {city, insufficient_materials}
                                    end
                            end;
                        false ->
                            Result = {city, invalid_structure_req}
                    end;
                false ->
                    Result = {city, invalid_item_type}
            end;
        false ->
            Result = {city, no_structure}
    end,

    {reply, Result, Data};

handle_call({'UPDATE_TAX', Taxes}, _From, Data) ->
    City = Data#module_data.city,

    NewCity = process_update_tax(City, Taxes),

    save_city(NewCity),
    Result = {success},

    NewData = Data#module_data {city = NewCity},

    {reply, Result, NewData};

handle_call({'ASSIGN_TASK', Caste, Race, Amount, TaskId, TaskType}, _From, Data) ->
    City = Data#module_data.city,
    log4erl:info("Assign task - Caste ~w Amount ~w Race ~w TaskId ~w TaskType ~w", [Caste, Amount, Race, TaskId, TaskType]), 
    PopAvailable = get_available_pop(City#city.id, Caste, Race),
    log4erl:info("PopAvailable: ~w", [PopAvailable]),
    
    case PopAvailable >= Amount of
        true ->
            case assignment:is_valid_task(City#city.id, TaskId, TaskType) of
                true ->
                    AssignmentId = assignment:add(City#city.id, 
                                                  Caste, 
                                                  Race, 
                                                  Amount,
                                                  TaskId, 
                                                  TaskType),
                    Result = {success, AssignmentId};
                false ->
                    log4erl:info("{~w}: Invalid TaskId", [?MODULE]),
                    Result = {failure, invalid_task}
            end;
        false ->
            Result = {failure, insufficient_pop}
    end,

    {reply, Result, Data};

handle_call({'REMOVE_TASK', AssignmentId}, _From, Data) ->
    City = Data#module_data.city,
    
    case db:read(assignment, AssignmentId) of
        [Assignment] ->
            case Assignment#assignment.city_id =:= City#city.id of
                true ->
                    db:delete(assignment, AssignmentId),
                    Result = {success, AssignmentId};
                false ->
                    log4erl:info("{~w} Assignment does not belong to city ~w", [?MODULE, City#city.id]),
                    Result = {failure, invalid_owner}
            end;
        _ ->
            Result = {failure, invalid_assignment}
    end,
   
    {reply, Result, Data};

handle_call({'GET_INFO', PlayerId}, _From, Data) ->    
    City = Data#module_data.city,
    
    if 
        City#city.player_id =:= PlayerId ->
            %Convert contracts record to tuple packet form 
            Contracts = db:dirty_index_read(contract, City#city.id, #contract.city_id),
            
            ?INFO("Processing contracts"),
            %Process contract production 
            contract:process_contracts(Contracts),           
 
            ?INFO("Contract tuple form"),
            %Convert contracts record to tuple packet form 
            NewContracts = db:dirty_index_read(contract, City#city.id, #contract.city_id),
            ContractsTuple = contract:tuple_form(NewContracts),
 
            %Convert assginments record to tuple packet form
            Assignments = db:dirty_index_read(assignment, City#city.id, #assignment.city_id),
            AssignmentsTuple = assignment:tuple_form(Assignments),

            %Convert units record to tuple packet form
            UnitsTuple = unit:tuple_form_items(PlayerId, City#city.id),

            %Convert buildings record to tuple packet form
            Buildings = db:dirty_index_read(building, City#city.id, #building.city_id),  
            BuildingsTuple = building:tuple_form(Buildings),

            ?INFO("Claims tuple form"),
            %Convert claims record to tuple packet form
            Claims = db:dirty_index_read(claim, City#city.id, #claim.city_id),
            ClaimsTuple = claim:tuple_form(Claims), 

            %Convert improvements record to tuple packet form
            NewImprovements = db:dirty_index_read(improvement, City#city.id, #improvement.city_id),
            ImprovementsTuple = improvements_tuple(NewImprovements),

            ?INFO("Items tuple form"),
            %Convert items record to tuple packet form
            Items = item:get_by_entity({?OBJECT_CITY, City#city.id}, PlayerId),
            ItemsTuple = item:tuple_form(Items),

            %Retrieve & convert population record to tuple packet form
            PopulationsTuple = population:tuple_form(City#city.id), 

            ?INFO("CityInfo tuple form"),
            CityInfo = {detailed, 
                        City#city.name,
                        City#city.tax_commoner,
                        City#city.tax_noble,
                        City#city.tax_tariff,
                        BuildingsTuple, 
                        UnitsTuple, 
                        ClaimsTuple,
                        ImprovementsTuple,
                        AssignmentsTuple,
                        ItemsTuple,
                        PopulationsTuple,
                        ContractsTuple};
        true ->
            CityInfo = {generic, 
                        City#city.id, 
                        City#city.player_id, 
                        City#city.name,
                        kingdom:get_name(City#city.player_id)}
    end,
    
    io:fwrite("city - CityInfo: ~w~n", [CityInfo]),
    {reply, CityInfo , Data};

handle_call({'TRANSFER_ITEM', ItemId, TargetId, TargetAtom}, _From, Data) ->
    log4erl:info("City - TRANSFER_ITEM"),
    City = Data#module_data.city,

    TargetPid = object:get_pid(TargetAtom, TargetId),
    case entity:on_same_tile(TargetPid, City#city.x, City#city.y) of
        true ->
            TargetPlayerId = entity:get_player_id(TargetPid), 
            item:transfer(ItemId, TargetId, TargetPlayerId),
            TransferItemInfo = {transfer_item, success};
        false ->
            TransferItemInfo = {transfer_item, not_same_tile}
    end,

    {reply, TransferItemInfo, Data};

handle_call({'TRANSFER_UNIT', _SourceId, UnitId, TargetId, TargetAtom}, _From, Data) ->   
    io:fwrite("city - transfer unit.~n"),
    City = Data#module_data.city,
    Units = City#city.units,
    
    case gb_sets:is_member(UnitId, Units) of
        true ->
            [Unit] = db:dirty_read(unit, UnitId),
            TargetPid = object:get_pid(TargetAtom, TargetId),

            case gen_server:call(TargetPid, {'ON_SAME_TILE', City#city.x, City#city.y}) of
                true ->
                    case gen_server:call(TargetPid, {'RECEIVE_UNIT', TargetId, Unit, Data#module_data.player_id}) of
                        {receive_unit, success} ->
                            db:dirty_delete(unit, Unit),
                            NewUnits = gb_sets:delete(UnitId, Units),
                            NewCity = City#city {units = NewUnits},
                            NewData = Data#module_data {city = NewCity},
                            TransferUnitInfo = {transfer_unit, success};
                        Error ->
                            TransferUnitInfo = Error,
                            NewData = Data
                    end;
                false ->            
                    TransferUnitInfo = {transfer_unit, not_same_tile},
                    NewData = Data
            end;
        false ->
            TransferUnitInfo = {transfer_unit, unit_is_not_member},
            NewData = Data
    end,

    save_city(NewData#module_data.city),    

    {reply, TransferUnitInfo , NewData};

handle_call({'RECEIVE_UNIT', _TargetId, Unit, PlayerId}, _From, Data) ->   
    ?INFO("Receive unit"),   
    City = Data#module_data.city,
    Units = City#city.units,
    
    if
        PlayerId =:= Data#module_data.player_id ->
            %Check to see if unit already exists
            UnitResult = gb_sets:is_member(Unit#unit.id, Units),
            
            if
                UnitResult =:= false ->                    
                    NewUnit = Unit#unit {entity_id = City#city.id},
                    db:dirty_write(NewUnit),

                    NewUnits = gb_sets:add(Unit#unit.id, Units),
                    NewCity = City#city {units = NewUnits},                
                    NewData = Data#module_data {city = NewCity},
                    ReceiveUnitInfo = {receive_unit, success};                
                true ->
                    NewData = Data,
                    ReceiveUnitInfo = {receive_unit, error}
            end;               
        true ->
            NewData = Data,
            ReceiveUnitInfo = {receive_unit, error}
    end,
    
    save_city(NewData#module_data.city),    

    {reply, ReceiveUnitInfo, NewData};   

handle_call({'GET_STATE', _CityId}, _From, Data) ->
    City = Data#module_data.city,
    
    State = #state {id = City#city.id,
                    player_id = City#city.player_id,
                    type = ?OBJECT_CITY,
                    subtype = ?OBJECT_BASIC,
                    state = City#city.state,
                    x = City#city.x,
                    y = City#city.y},
    
    {reply, State, Data};

handle_call({'GET_COORDS'}, _From, Data) ->
    City = Data#module_data.city,
    {reply, {City#city.x, City#city.y}, Data};

handle_call({'GET_ID'}, _From, Data) ->
    City = Data#module_data.city,
    {reply, City#city.id, Data};

handle_call({'GET_PLAYER_ID'}, _From, Data) ->
    {reply, Data#module_data.player_id, Data};

handle_call({'GET_TYPE', _CityId}, _From, Data) ->
    {reply, ?OBJECT_CITY, Data};

handle_call('GET_INVENTORY', _From, Data) ->
    {reply, Data#module_data.inventory, Data};

handle_call('GET_VISIBLE', _From, Data) ->
    {reply, Data#module_data.visible, Data};

handle_call('GET_OBSERVED_BY', _From, Data) ->
    {reply, Data#module_data.observed_by, Data};

handle_call('GET_SUBSCRIPTION_DATA', _From, Data) ->
    City = Data#module_data.city,
    {reply, {City#city.x, City#city.y, Data#module_data.visible, Data#module_data.observed_by}, Data};

handle_call({'ON_SAME_TILE', X, Y}, _From, Data) -> 
    City = Data#module_data.city,
    {reply, (City#city.x =:= X) and (City#city.y =:= Y), Data};

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
%%
%% Local Functions
%%
process_update_tax(City, []) ->
    City;

process_update_tax(City, [Tax | Rest]) ->
    {InputRate, Type} = Tax,
    
    case InputRate of 
        R when R > 100 -> NewRate = 100;
        R when R < 0 -> NewRate = 0;
        _ -> NewRate = InputRate
    end,

    case Type of
        ?TAX_COMMONER ->
            NewCity = City#city {tax_commoner = NewRate};
        ?TAX_NOBLE ->
            NewCity = City#city {tax_noble = NewRate};
        ?TAX_TARIFF ->
            NewCity = City#city {tax_tariff = NewRate};
        _ ->
            ?INFO("Invalid tax type: ", Type),
            NewCity = City
    end,

    process_update_tax(NewCity, Rest).

income(City) ->
    Population = db:dirty_index_read(population, City#city.id, #population.city_id),    
     
    Tax = population_tax(Population, City, 0),

    kingdom:update_gold(City#city.player_id, Tax).

population_tax([], _City, Tax) ->
    Tax;

population_tax([Caste | Rest], City, Tax) ->
    CasteTax = caste_tax(Caste#population.caste, City),
    NewTax = Tax + (Caste#population.value * CasteTax / 100),
    population_tax(Rest, City, NewTax). 

caste_tax(?CASTE_SLAVE, _City) ->
    0;
caste_tax(?CASTE_SOLDIER, _City) ->
    0;
caste_tax(?CASTE_COMMONER, City) ->
    City#city.tax_commoner * ?CASTE_COMMONER_REVENUE;
caste_tax(?CASTE_NOBLE, City) ->
    City#city.tax_noble * ?CASTE_NOBLE_REVENUE.

growth(CityId, PlayerId) ->
    Population = db:dirty_index_read(population, CityId, #population.city_id),    
    log4erl:debug("{~w} Growth Current Population: ~w", [?MODULE, Population]),

    %Calculate food required
    TotalFoodRequired = total_food_required(Population, 0),
    
    %Subtract from food stores
    food_upkeep(CityId, PlayerId, Population, TotalFoodRequired).


food_required(?CASTE_SLAVE) ->
    1;
food_required(?CASTE_SOLDIER) ->
    2;
food_required(?CASTE_COMMONER) ->
    2;
food_required(?CASTE_NOBLE) ->
    4.

growth_rate(?CASTE_SLAVE) ->
    1.03;
growth_rate(?CASTE_SOLDIER) ->
    1.02;
growth_rate(?CASTE_COMMONER) ->
    1.01;
growth_rate(?CASTE_NOBLE) ->
    1.005.

total_food_required([], TotalFoodRequired) ->
    TotalFoodRequired;

total_food_required([Caste | Rest], FoodRequired) ->
    NewFoodRequired = food_required(Caste#population.caste) * Caste#population.value,
    TotalFoodRequired = FoodRequired + NewFoodRequired,

    total_food_required(Rest, TotalFoodRequired).

food_upkeep(CityId, PlayerId, Population, TotalFoodRequired) ->
    case get_food_items(CityId, PlayerId) of
        [] ->
            starve_population(Population, TotalFoodRequired);
        FoodList ->
            log4erl:info("Food upkeep: ~w",[FoodList]),
            TotalFood = total_food(FoodList, 0),
            if
                TotalFood >= TotalFoodRequired ->
                    log4erl:info("Add population..."),
                    grow_population(Population);
                true ->
                    starve_population(Population, TotalFoodRequired - TotalFood)
            end,
            log4erl:info("FoodList: ~w TotalFoodRequired: ~w", [FoodList, TotalFoodRequired]),            
            remove_food(FoodList, TotalFoodRequired)
    end.         

get_food_items(CityId, PlayerId) ->
    FoodCategoryList = [?ITEM_FOOD],

    F = fun(FoodCategory, FoodItemList) ->
            ?INFO("Food Category: ", FoodCategory),
            {true, FoodItem} = item:get_by_type({?OBJECT_CITY, CityId}, PlayerId, FoodCategory),
            [FoodItem | FoodItemList]
        end,

    lists:foldl(F, [], FoodCategoryList).

total_food([], TotalFood) ->
    TotalFood;
    
total_food([Food | Rest], TotalFood) ->
    NewTotalFood = TotalFood + Food#item.volume,
    total_food(Rest, NewTotalFood).

remove_food(_FoodList, 0) ->
    done;

remove_food([], _FoodRequired) ->
    done;

remove_food([Food | Rest], FoodRequired) ->
    if
        Food#item.volume > FoodRequired ->
            NewFood = Food#item {volume = Food#item.volume - FoodRequired},
            NewFoodRequired = 0,
            item:set(NewFood#item.id, round(NewFood#item.volume));
        true ->
            NewFoodRequired = FoodRequired - Food#item.volume,
            item:delete(Food#item.id)
    end,
    remove_food(Rest, NewFoodRequired).
        
grow_population([]) ->
    ok;

grow_population([Caste | Rest]) ->
    GrowthRate = growth_rate(Caste#population.caste),
    NewValue = Caste#population.value * GrowthRate,   
    NewCaste = Caste#population {value = util:round3(NewValue)},
    db:write(NewCaste),

    grow_population(Rest).

starve_population(Population, InsufficientFood) ->   
    SortedPopulation = lists:keysort(#population.caste, Population),
    starve_caste(SortedPopulation, InsufficientFood). 

starve_caste([], _InsufficentFood) ->
    log4erl:info("Starvation completed.");

starve_caste([Caste | Rest], InsufficientFood) ->
    log4erl:info("Caste: ~w", [Caste]),
    log4erl:info("InsufficientFood: ~w", [InsufficientFood]),

    CasteFoodRequired = food_required(Caste#population.caste),
    TotalCasteFoodRequired = Caste#population.value * CasteFoodRequired,

    if
        TotalCasteFoodRequired >= InsufficientFood ->
            NewInsufficientFood = 0,
            NewValue = (TotalCasteFoodRequired - InsufficientFood) / CasteFoodRequired;
        true ->
            NewInsufficientFood = InsufficientFood - TotalCasteFoodRequired,
            NewValue = 0
    end,

    NewCaste = Caste#population {value = util:round3(NewValue)},
    db:write(NewCaste),
    log4erl:info("NewInsufficientFood: ~w",[NewInsufficientFood]),
    starve_caste(Rest, NewInsufficientFood).

get_available_pop(CityId, Caste, Race) ->
    log4erl:info("GetAvailablePop CityId: ~w Caste: ~w Race: ~w",[CityId, Caste, Race]),
    Assignments = db:dirty_match_object({assignment, '_', CityId, Caste, Race, '_', '_'}),
    log4erl:info("Assignments: ~w", [Assignments]),
    
    [TotalPop] = db:dirty_read(population, {CityId, Caste, Race}),
    TotalAssignment = total_assignment(Assignments, 0),
    TotalPop#population.value - TotalAssignment.   

total_assignment([], TotalAssignment) ->
    TotalAssignment;

total_assignment([Assignment | Rest], TotalAssignment) ->
    NewTotalAssignment = Assignment#assignment.amount + TotalAssignment,
    total_assignment(Rest, NewTotalAssignment).

improvements_tuple(Improvements) ->
    F = fun(Improvement, ImprovementList) ->
            ImprovementTuple = {Improvement#improvement.id, 
                                Improvement#improvement.type},
            [ImprovementTuple | ImprovementList]
        end,
    lists:foldl(F, [], Improvements).

save_city(City) ->
    io:fwrite("~w ~n",[City]),	
    db:dirty_write(City).

update_perception(PlayerId) ->
    case player:get_type(PlayerId) of
        ?PLAYER_HUMAN ->
            %Toggle within game state that player's perception has been updated.
            gen_server:cast(global:whereis_name(game_pid),{'UPDATE_PERCEPTION', PlayerId});
        ?PLAYER_COMPUTER ->
            no_update;
        _PlayerType ->
            no_update
    end.
