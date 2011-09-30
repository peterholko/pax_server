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
         queue_building/3, 
         queue_improvement/4,
         queue_item/5, 
         add_claim/4, 
         remove_claim/2, 
         assign_task/6,
         remove_task/2]).

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

queue_unit(CityId, PlayerId, UnitType, UnitSize, Caste, Race) ->
    gen_server:call(global:whereis_name({city, CityId}), {'QUEUE_UNIT', PlayerId, UnitType, UnitSize, Caste, Race}).

queue_building(CityId, PlayerId, BuildingType) ->
    gen_server:call(global:whereis_name({city, CityId}), {'QUEUE_BUILDING', PlayerId, BuildingType}).

queue_improvement(CityId, X, Y, ImprovementType) ->
    gen_server:call(global:whereis_name({city,CityId}), {'QUEUE_IMPROVEMENT', X, Y, ImprovementType}).

queue_item(CityId, SourceId, SourceType, ItemType, ItemSize) ->
    gen_server:call(global:whereis_name({city,CityId}), {'QUEUE_ITEM', SourceId, SourceType, ItemType, ItemSize}).

add_claim(CityId, ArmyId, X, Y) ->
    gen_server:call(global:whereis_name({city, CityId}), {'ADD_CLAIM', ArmyId, X, Y}).

remove_claim(CityId, ClaimId) ->
    gen_server:call(global:whereis_name({city, CityId}), {'REMOVE_CLAIM', ClaimId}).

assign_task(CityId, Caste, Race, Amount, TaskId, TaskType) ->
    gen_server:call(global:whereis_name({city, CityId}), {'ASSIGN_TASK', Caste, Race, Amount, TaskId, TaskType}).

remove_task(CityId, AssignmentId) ->
    gen_server:call(global:whereis_name({city, CityId}), {'REMOVE_TASK', AssignmentId}).

%%
%% OTP handlers
%%

handle_cast({'ADD_VISIBLE', _CityId, EntityId, EntityPid}, Data) ->
    VisibleList = Data#module_data.visible,
    NewVisibleList = [{EntityId, EntityPid} | VisibleList],
    NewData = Data#module_data { visible = NewVisibleList },
    
    {noreply, NewData};

handle_cast({'REMOVE_VISIBLE', _CityId, EntityId, EntityPid}, Data) ->
    VisibleList = Data#module_data.visible,
    NewVisibleList = lists:delete({EntityId, EntityPid}, VisibleList),
    NewData = Data#module_data { visible = NewVisibleList },
    
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

handle_call({'QUEUE_UNIT', PlayerId, BuildingId, UnitType, UnitSize, Caste, Race}, _From, Data) ->   
    City = Data#module_data.city,

    UnitCost = unit:calc_unit_cost(UnitType, UnitSize), 
    CasesUnitQueue = cases_unit_queue(PlayerId =:= City#city.player_id, 
                                      unit:is_valid_unit_type(UnitType),                             
                                      kingdom:get_gold(PlayerId) >= UnitCost,
                                      get_available_pop(City#city.id, Caste, Race) >= UnitSize),
      
    case CasesUnitQueue of
        true ->                        
            case building:is_available(BuildingId) of
               {true, Building} ->
                    kingdom:remove_gold(PlayerId, UnitCost),
                    remove_population(City#city.id, Caste, Race, UnitSize),                        

                    unit:add_to_queue(City#city.id, BuildingId, UnitType, UnitSize),
                    Result = {city, queued_unit};
               false ->                    
                    ?INFO("Building not found"),
                    Result = {city, "Building not found"}
            end;
        {false, ErrorMsg} ->
            ?ERROR("QueueUnit Error", ErrorMsg),
            Result = {city, ErrorMsg}
    end,
    
    {reply, Result, Data};

handle_call({'QUEUE_BUILDING', PlayerId, BuildingType}, _From, Data) ->
    ?INFO("Queue Building Type", BuildingType),
    City = Data#module_data.city,        
    IsValid = building:is_valid(PlayerId =:= City#city.player_id,
                               building:check_type(BuildingType)),

    case IsValid of
        true ->                        
            building:add_to_queue(City#city.id, BuildingType),
            
            BuildingCost = building:calc_gold_cost(BuildingType),
            kingdom:remove_gold(PlayerId, BuildingCost),

            Result = {city, queued_building};
        {false, ErrorType} ->
            Result = {city, ErrorType}
    end,

    {reply, Result, Data}; 

handle_call({'QUEUE_IMPROVEMENT', X, Y, ImprovementType}, _From, Data) ->
    City = Data#module_data.city,
    TileIndex = map:convert_coords(X,Y),

    CheckClaimStatus = claim:check_status(TileIndex), 
    CheckEmpty = improvement:empty(X,Y), 
    CheckType = improvement:check_type(ImprovementType),

    IsValid = improvement:is_valid(CheckClaimStatus,
                                   CheckEmpty,
                                   CheckType),

    case IsValid of
        true ->
            ImprovementPid = global:whereis_name(improve_pid),
            ImprovementId = counter:increment(entity),
            improvement:queue(ImprovementId, X, Y, Data#module_data.player_id, City#city.id, ImprovementType),
            
            %Subscription update
            {ok, SubPid} = subscription:start(ImprovementId),
            subscription:update_perception(SubPid, ImprovementId, ImprovementPid, X, Y, [], []),

            %Toggle player's perception has been updated.
            game:update_perception(Data#module_data.player_id),

            Result = {city, queued_improvement};
        {false, ErrorType} ->
            log4erl:info("{~w} Queue Improvement failed: ~w", [?MODULE, ErrorType]),
            Result = {city, ErrorType}
            
    end,

    {reply, Result, Data};

handle_call({'QUEUE_ITEM', SourceId, SourceType, ItemType, ItemSize}, _From, Data) ->
    City = Data#module_data.city,

    case SourceType of 
        ?OBJECT_IMPROVEMENT ->
            case improvement:is_available(SourceId) of
                {true, Improvement} ->
                    case item:check_type(ItemType) of
                        true -> 
                            item:add_to_queue(City#city.player_id,
                                              City#city.id,
                                              ?CONTRACT_HARVEST,
                                              {SourceId, ?OBJECT_IMPROVEMENT},
                                              -1,
                                              ItemSize),
                            Result = {city, queued_harvest};
                        false ->
                            Result = {city, invalid_item_type}
                    end;
                false ->
                    Result = {city, invalid_improvement}
            end;
        ?OBJECT_BUILDING ->
            case building:is_available(SourceId) of
                {true, Building} ->
                    case item:check_type(ItemType) of
                        true ->
                            item:add_to_queue(City#city.player_id,
                                              City#city.id,
                                              ?CONTRACT_ITEM,
                                              {SourceId, ?OBJECT_BUILDING},
                                              ItemType,
                                              ItemSize),
                            Result = {city, queued_item};
                        false ->
                            Result = {city, invalid_item_type}
                    end;
                false ->
                    Result = {city, invalid_building}
            end;
        _ ->
            Result = {city, invalid_source_type}
    end,

    {reply, Result, Data};

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
            
            %Process contract production 
            contract:process_contracts(Contracts),           
 
            %Convert contracts record to tuple packet form 
            NewContracts = db:dirty_index_read(contract, City#city.id, #contract.city_id),
            ContractsTuple = contract:tuple_form(NewContracts),
 
            %Convert assginments record to tuple packet form
            Assignments = db:dirty_index_read(assignment, City#city.id, #assignment.city_id),
            AssignmentsTuple = assignment:tuple_form(Assignments),

            %Convert units record to tuple packet form
            Units = db:dirty_index_read(unit, City#city.id, #unit.entity_id),
            UnitsTuple = unit:tuple_form(Units),

            %Convert buildings record to tuple packet form
            Buildings = db:dirty_index_read(building, City#city.id, #building.city_id),  
            BuildingsTuple = building:tuple_form(Buildings),

            %Convert claims record to tuple packet form
            Claims = db:dirty_index_read(claim, City#city.id, #claim.city_id),
            ClaimsTuple = claim:tuple_form(Claims), 

            %Convert improvements record to tuple packet form
            NewImprovements = db:dirty_index_read(improvement, City#city.id, #improvement.city_id),
            ImprovementsTuple = improvements_tuple(NewImprovements),

            %Convert items record to tuple packet form
            Items = db:dirty_index_read(item, {City#city.id, PlayerId}, #item.ref),
            ItemsTuple = item:tuple_form(Items),

            %Convert population record to tuple packet form
            NewPopulations = db:dirty_index_read(population, City#city.id, #population.city_id),
            PopulationsTuple = populations_tuple(NewPopulations), 

            CityInfo = {detailed, 
                        City#city.name,
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
                            db:dirty_delete(Unit),
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

handle_call({'GET_ID'}, _From, Data) ->
    City = Data#module_data.city,
    {reply, City#city.id, Data};

handle_call({'GET_PLAYER_ID'}, _From, Data) ->
    {reply, Data#module_data.player_id, Data};

handle_call({'GET_TYPE'}, _From, Data) ->
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
    case item:get_by_type(CityId, PlayerId, ?ITEM_FOOD) of
        {found, FoodList} ->
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
            remove_food(FoodList, TotalFoodRequired);
        {not_found} ->
            starve_population(Population, TotalFoodRequired)
    end.         

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

remove_population(CityId, Caste, Race, Value) ->
    [Population] = db:dirty_read(population, {CityId, Caste, Race}),
    NewValue = Population#population.value - Value, 
    NewPopulation = Population#population { value = NewValue},
    db:dirty_write(NewPopulation).

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

          
cases_unit_queue(_IsPlayer = true, _IsValidUnitType = true, _CanAfford = true, _PopAvailable = true) ->
    true;
cases_unit_queue(_IsPlayer = false, _IsValidUnitType, _CanAfford, _PopAvailable) ->
    {false, not_player};
cases_unit_queue(_IsPlayer, _IsValidUnitType = true, _CanAfford, _PopAvailable) ->
    {false, invalid_unit_type};
cases_unit_queue(_IsPlayer, _IsValidUnitType, _CanAfford = true, _PopAvailable) ->
    {false, insufficient_gold};
cases_unit_queue(_IsPlayer, _IsValidUnitType, _CanAfford, _PopAvailable = true) ->
    {false, insufficient_pop}.

improvements_tuple(Improvements) ->
    F = fun(Improvement, ImprovementList) ->
            ImprovementTuple = {Improvement#improvement.id, 
                                Improvement#improvement.type},
            [ImprovementTuple | ImprovementList]
        end,
    lists:foldl(F, [], Improvements).

populations_tuple(Populations) ->
    F = fun(Population, PopulationList) ->
        PopulationTuple = {Population#population.city_id,
                           Population#population.caste,
                           Population#population.race,
                           erlang:trunc(Population#population.value)},
        [PopulationTuple | PopulationList]
    end,
    lists:foldl(F, [], Populations).

save_city(City) ->
    db:dirty_write(City).

