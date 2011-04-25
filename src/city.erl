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

-export([start/2, stop/1, queue_unit/5, queue_building/3, add_claim/3, add_improvement/4, assign_task/5]).

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
    
    %UnitQueue setup
    UnitsQueue = db:index_read(unit_queue, City#city.id, #unit_queue.city_id),

    %Inventory setup    
    Inventory = dict:new(),

    {ok, #module_data{city = City, 
                      units_queue = UnitsQueue,
                      inventory = Inventory, 
                      player_id = PlayerId, 
                      self = self() }}.

terminate(_Reason, _) ->
    ok.

stop(ProcessId) 
  when is_pid(ProcessId) ->
    gen_server:cast(ProcessId, stop).

queue_unit(CityId, PlayerId, UnitType, UnitSize, Caste) ->
    gen_server:call(global:whereis_name({city, CityId}), {'QUEUE_UNIT', PlayerId, UnitType, UnitSize, Caste}).

queue_building(CityId, PlayerId, BuildingType) ->
    gen_server:call(global:whereis_name({city, CityId}), {'QUEUE_BUILDING', PlayerId, BuildingType}).

add_improvement(CityId, X, Y, ImprovementType) ->
    gen_server:cast(global:whereis_name({city,CityId}), {'ADD_IMPROVEMENT', X, Y, ImprovementType}).

add_claim(CityId, X, Y) ->
    gen_server:call(global:whereis_name({city, CityId}), {'ADD_CLAIM', X, Y}).

assign_task(CityId, PopulationId, Amount, TaskId, TaskType) ->
    gen_server:call(global:whereis_name({city, CityId}), {'ASSIGN_TASK', PopulationId, Amount, TaskId, TaskType}).
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

handle_cast({'ADD_UNIT', UnitId}, Data) ->
    City = Data#module_data.city,
    Units = City#city.units,
    NewUnits = [UnitId | Units],
    NewCity = City#city { units = NewUnits},
    NewData = Data#module_data {city = NewCity},
    save_city(NewCity),    
 
    {noreply, NewData};

handle_cast({'ADD_IMPROVEMENT', X, Y, ImprovementType}, Data) ->
    City = Data#module_data.city,

    CheckClaim = lists:member({X,Y}, City#city.claims),
    CheckEmpty = improvement:empty(X,Y), 

    case check_improvement(CheckClaim, CheckEmpty) of
        true ->
            ImprovementPid = global:whereis_name(improve_pid),
            ImprovementId = counter:increment(entity),
            improvement:create(ImprovementId, X, Y, Data#module_data.player_id, City#city.id, ImprovementType),
            
            NewImprovements = [ImprovementId | Data#module_data.improvements],
            NewData = Data#module_data { improvements = NewImprovements},

            %Subscription update
            EveryObject = gen_server:call(global:whereis_name(game_pid), 'GET_OBJECTS'),
            {ok, SubPid} = subscription:start(ImprovementId),
            subscription:update_perception(SubPid, ImprovementId, ImprovementPid, X, Y, EveryObject, [], []),

            %Toggle player's perception has been updated.
            game:update_perception(Data#module_data.player_id);

        false ->
            log4erl:info("Add Improvement - Tile is not claimed by this city."),
            NewData = Data
    end,

    {noreply, NewData};

handle_cast({'PROCESS_EVENT',_EventTick, _Id, EventType}, Data) ->   
    City = Data#module_data.city,

    case EventType of
        ?EVENT_HARVEST -> 
            %log4erl:info("Processing Harvest for City ~w~n", [self()]),
            harvest(City#city.id);
        ?EVENT_GROWTH ->
            growth(City#city.id, City#city.player_id)            
    end,      
    
    
    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({'ADD_CLAIM', X, Y}, _From, Data) ->
    City = Data#module_data.city,
    TileIndex = map:convert_coords(X, Y),
       
    ValidX = abs(City#city.x - X) =< 6,
    ValidY = abs(City#city.y - Y) =< 6,
    Exists = length(db:index_read(claim, TileIndex, #claim.tile_index)) > 0,
    MaxReached = length(City#city.claims) > 3,

    log4erl:debug("ValidX: ~w ValidY: ~w Exists: ~w MaxReached: ~w~n", [ValidX, ValidY, Exists, MaxReached]), 

    case check_claim(ValidX, ValidY, Exists, MaxReached) of
        true ->
            log4erl:info("Add claim successful."),
            ClaimId = counter:increment(claim),
            NewClaimRecord = #claim {id = ClaimId,
                                     tile_index = TileIndex,
                                     city_id = City#city.id},
            db:write(NewClaimRecord),
            
            NewClaims = [{X, Y} | City#city.claims],
            NewCity = City#city { claims = NewClaims },
            Result = {success, ClaimId},
            NewData = Data#module_data { city = NewCity };
        false ->
            Result = {failure, "Check Failed"},
            NewData = Data
    end,

    save_city(NewData#module_data.city),

    {reply, Result, NewData};

handle_call({'QUEUE_UNIT', PlayerId, UnitType, UnitSize, Caste}, _From, Data) ->   
    City = Data#module_data.city,
    UnitsQueue = Data#module_data.units_queue,

    UnitCost = unit:calc_new_unit_cost(UnitType, UnitSize), 

    CasesUnitQueue = cases_unit_queue(PlayerId =:= City#city.player_id, 
                                      unit:is_valid_unit_type(UnitType),                             
                                      kingdom:get_gold(PlayerId) >= UnitCost,
                                      get_available_pop(City#city.id, Caste) >= UnitSize),
      
    case CasesUnitQueue of
        true ->                          
            kingdom:remove_gold(PlayerId, UnitCost),
            remove_population(City#city.id, Caste, UnitSize),                        

            NewUnitsQueue = add_unit_to_queue(City#city.id, UnitsQueue, UnitType, UnitSize),
            NewData = Data#module_data { units_queue = NewUnitsQueue},
            Result = {city, queued_unit};
        {false, ErrorMsg} ->
            NewData = Data,
            io:fwrite("QueueUnit Error: ~w~n", [ErrorMsg]),
            Result = {city, ErrorMsg}
    end,
    
    {reply, Result, NewData};

handle_call({'QUEUE_BUILDING', PlayerId, BuildingType}, _From, Data) ->
    City = Data#module_data.city,
    BuildingsQueue = db:dirty_index_read(building_queue, City#city.id, #building_queue.city_id),
        
    case City#city.player_id =:= PlayerId of
        true ->
            add_building_to_queue(City#city.id, BuildingsQueue, BuildingType),
            Result = {city, queued_building};
        false ->
            Result = {city, error}
    end,

    {reply, Result, Data}; 

handle_call({'ASSIGN_TASK', Caste, Amount, TaskId, TaskType}, _From, Data) ->
    City = Data#module_data.city,
    log4erl:info("Assign task - Caste ~w Amount ~w TaskId ~w TaskType ~w", [Caste, Amount, TaskId, TaskType]), 
    PopAvailable = get_available_pop(City#city.id, Caste),
    log4erl:info("PopAvailable: ~w", [PopAvailable]),
    
    case PopAvailable >= Amount of
        true ->
            case is_valid_task(City#city.id, TaskId, TaskType) of
                true ->          
                    process_assignment(City#city.id, Caste, Amount, TaskId, TaskType),
                    AssignmentId = store_assigned_task(City#city.id, Caste, Amount, TaskId, TaskType),
                    Result = {success, AssignmentId};
                false ->
                    log4erl:info("~w: Invalid TaskId", [?MODULE]),
                    Result = {failure, invalid_task}
            end;
        false ->
            Result = {failure, insufficient_pop}
    end,

    {reply, Result, Data};


handle_call({'GET_INFO', PlayerId}, _From, Data) ->    
    City = Data#module_data.city,
    
    if 
        City#city.player_id =:= PlayerId ->           
            %Get UnitQueue and Units
            UnitsQueue = Data#module_data.units_queue,
            UnitIds = City#city.units,

            %Get BuildingQueue and buildings,
            BuildingsQueue = db:dirty_index_read(building_queue, City#city.id, #building_queue.city_id),
            BuildingIds = City#city.buildings,
            
            log4erl:info("BuildingsQueue: ~w~n", [BuildingsQueue]),

            %Check if any units or buildings are completed
            {NewUnitsQueue, UnitsToAdd} = check_unit_queue(UnitsQueue),
            {NewBuildingsQueue, BuildingsToAdd} = check_building_queue(BuildingsQueue),
          
            log4erl:info("NewBuildingsQueue: ~w~n", [NewBuildingsQueue]),
  
            %Add any units from the queue
            NewUnitIds = add_units_from_queue(UnitIds, UnitsToAdd),
            NewBuildingIds = add_buildings_from_queue(BuildingIds, BuildingsToAdd),
            
            %Assign any changes to module data
            NewCity = City#city {units = NewUnitIds, buildings = NewBuildingIds}, 
            NewData = Data#module_data {city = NewCity, units_queue = NewUnitsQueue},                                    

            %Convert units record to tuple packet form
            NewUnits = db:dirty_index_read(unit, City#city.id, #unit.entity_id),
            UnitsTuple = unit:units_tuple(NewUnits),
            UnitsQueueTuple = unit:units_queue_tuple(NewUnitsQueue),

            %Convert buildings record to tuple packet form
            NewBuildings = db:dirty_index_read(building, City#city.id, #building.city_id),  
            BuildingsTuple = building:buildings_tuple(NewBuildings),
            BuildingsQueueTuple = building:buildings_queue_tuple(NewBuildingsQueue),       

            %Convert claims record to tuple packet form
            NewClaims = db:dirty_index_read(claim, City#city.id, #claim.city_id),
            ClaimsTuple = claims_tuple(NewClaims), 

            %Convert improvements record to tuple packet form
            NewImprovements = db:dirty_index_read(improvement, City#city.id, #improvement.city_id),
            ImprovementsTuple = improvements_tuple(NewImprovements),

            %Convert assginments record to tuple packet form
            NewAssignments = db:dirty_index_read(assignment, City#city.id, #assignment.city_id),
            AssignmentsTuple = assignments_tuple(NewAssignments),

            %Convert items record to tuple packet form
            NewItems = db:dirty_index_read(item, {City#city.id, PlayerId}, #item.ref),
            ItemsTuple = item:items_tuple(NewItems),

            NewPopulations = db:dirty_index_read(population, City#city.id, #population.city_id),
            PopulationsTuple = populations_tuple(NewPopulations), 

            %Save city record
            save_city(NewCity),
            
            CityInfo = {detailed, 
                        City#city.name,
                        BuildingsTuple, 
                        BuildingsQueueTuple, 
                        UnitsTuple, 
                        UnitsQueueTuple,
                        ClaimsTuple,
                        ImprovementsTuple,
                        AssignmentsTuple,
                        ItemsTuple,
                        PopulationsTuple};
        true ->
            NewData = Data,
            CityInfo = {generic, 
                        City#city.id, 
                        City#city.player_id, 
                        City#city.name,
                        kingdom:get_name(City#city.player_id)}
    end,
    
    io:fwrite("city - CityInfo: ~w~n", [CityInfo]),
    {reply, CityInfo , NewData};

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
    io:fwrite("city - receive unit.~n"),   
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

add_unit_to_queue(CityId, UnitsQueue, UnitType, UnitSize) ->
    CurrentTime = util:get_time_seconds(),
    StartTime = get_queue_unit_time(UnitsQueue, CurrentTime),   
    io:fwrite("city - db_queue_unit - StartTime: ~w~n", [StartTime]),
    
    UnitQueue = #unit_queue {id = counter:increment(unit_queue),
                             city_id = CityId,
                             unit_type = UnitType,
                             unit_size = UnitSize,
                             start_time = StartTime,
                             end_time = StartTime + 30},
    
    [UnitQueue | UnitsQueue].

get_queue_unit_time([], CurrentTime) ->
    CurrentTime;

get_queue_unit_time(QueueInfo, CurrentTime) ->
    io:fwrite("city - get_queue_unit_time - QueueInfo: ~w~n", [QueueInfo]),
    SortedQueueInfo = lists:keysort(5, QueueInfo),    
    LastUnitQueue = lists:last(SortedQueueInfo),  
    EndTime = LastUnitQueue#unit_queue.end_time,
    
    if
        EndTime > CurrentTime ->
            StartTime = EndTime;
        true ->
            StartTime = CurrentTime
    end,
    StartTime.

add_building_to_queue(CityId, BuildingsQueue, BuildingType) ->
    CurrentTime = util:get_time_seconds(),
    StartTime = get_queue_building_time(BuildingsQueue, CurrentTime),
    
    BuildingQueue = #building_queue {id = counter:increment(building_queue),
                                     city_id = CityId,
                                     building_type = BuildingType,
                                     start_time = StartTime,
                                     end_time = ?MAX_TIME},
    db:dirty_write(BuildingQueue),
    [BuildingQueue | BuildingsQueue].

get_queue_building_time([], CurrentTime) ->
    CurrentTime;

get_queue_building_time(QueueInfo, CurrentTime) ->
    SortedQueueInfo = lists:keysort(4, QueueInfo),
    LastBuildingQueue = lists:last(SortedQueueInfo),
    EndTime = LastBuildingQueue#building_queue.end_time,
    
    case EndTime > CurrentTime of
        true ->
            StartTime = EndTime;
        false ->
            StartTime = CurrentTime
    end,
    StartTime.

check_unit_queue([]) ->
    {[], []};

check_unit_queue(QueueInfo) ->
    CurrentTime = util:get_time_seconds(),
    
    io:fwrite("city - check_unit_queue - CurrentTime: ~w~n", [CurrentTime]),  
    
    F = fun(UnitQueue, UnitsToRemove) ->
                EndTime = UnitQueue#unit_queue.end_time,
                
                if
                    EndTime =< CurrentTime ->    
                        io:fwrite("city - check_unit_queue - UnitQueue: ~w~n", [UnitQueue]),    
                        NewUnitsToRemove = [UnitQueue | UnitsToRemove];
                    true ->
                        NewUnitsToRemove = UnitsToRemove
                end,
                
                NewUnitsToRemove
        end,
    
    UnitsToRemove = lists:foldl(F, [], QueueInfo),
    
    NewQueueInfo = QueueInfo -- UnitsToRemove,
    io:fwrite("city - check_unit_queue - UnitsRemoved: ~w~n", [UnitsToRemove]),    
    {NewQueueInfo, UnitsToRemove}.  

check_building_queue([]) ->
    {[], []};

check_building_queue(QueueInfo) ->
    CurrentTime = util:get_time_seconds(),

    F = fun(BuildingQueue, BuildingsToRemove) ->
            EndTime = BuildingQueue#building_queue.end_time,

            case EndTime =< CurrentTime of
                true ->
                    NewBuildingsToRemove = [BuildingQueue | BuildingsToRemove];
                false ->
                    NewBuildingsToRemove = BuildingsToRemove
            end,
            
            NewBuildingsToRemove
        end,

    BuildingsToRemove = lists:foldl(F, [], QueueInfo),
    NewQueueInfo = QueueInfo -- BuildingsToRemove,
    {NewQueueInfo, BuildingsToRemove}.

add_units_from_queue(Units, []) ->
    Units;

add_units_from_queue(Units, UnitsToAdd) ->    
    [UnitQueue | Rest] = UnitsToAdd,
    
    Unit = #unit {id = counter:increment(unit),
                  entity_id = UnitQueue#unit_queue.city_id,
                  entity_type = ?OBJECT_CITY,
                  type = UnitQueue#unit_queue.unit_type,
                  size = UnitQueue#unit_queue.unit_size},
    
    db:dirty_write(Unit),
    NewUnits = [Unit#unit.id | Units],        
    add_units_from_queue(NewUnits, Rest).

add_buildings_from_queue(Buildings, []) ->
    Buildings;

add_buildings_from_queue(Buildings, BuildingsToAdd) ->
    [BuildingQueue | Rest] = BuildingsToAdd,
    
    Building = #building {id = counter:increment(building),
                          city_id = BuildingQueue#building_queue.city_id,
                          type = BuildingQueue#building_queue.building_type},
    
    db:dirty_write(Building),
    db:dirty_delete(building_queue, BuildingQueue#building_queue.id),
    NewBuildings = [Building#building.id | Buildings],

    add_buildings_from_queue(NewBuildings, Rest).

%Valid X, Valid Y, Exists, MaxReached
check_claim(true, true, false, false) ->
    true;
check_claim(_, _, _, _) ->
    log4erl:info("Add claim failed."),
    false.

check_improvement(true, true) ->
    true;
check_improvement(_, _) ->
    false.

harvest(CityId) ->
    log4erl:info("{~w} Harvest CityId: ~w", [?MODULE, CityId]),
    % match {assignment, assignment_id, city_id, population_id, amount, task_id, task_type}
    Assignments = db:dirty_match_object({assignment, '_', CityId, '_', '_', '_', ?TASK_IMPROVEMENT}),
    harvest_assignment(Assignments).
            
harvest_assignment([]) ->
    log4erl:info("{~w} Harvest Assignment complete", [?MODULE]);

harvest_assignment([Assignment | Rest]) ->
    log4erl:info("{~w} Harvest Assignment", [?MODULE]),
    ImprovementId = Assignment#assignment.task_id,
    [Improvement] = db:dirty_read(improvement, ImprovementId),
    ResourceGained  = map:harvest_resource(Improvement#improvement.tile_index, 0, Assignment#assignment.amount),

    log4erl:info("{~w} Harvest ResourceGained: ~w",[?MODULE, ResourceGained]),
    item:create(Improvement#improvement.city_id, 
                Improvement#improvement.player_id, 
                Improvement#improvement.type, 
                ResourceGained),

    harvest_assignment(Rest).

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

remove_population(CityId, Caste, Value) ->
    [Population] = db:dirty_read(population, {CityId, Caste}),
    NewValue = Population#population.value - Value, 
    NewPopulation = Population#population { value = NewValue},
    db:dirty_write(NewPopulation).

starve_population(Population, InsufficientFood) ->   
    [Slaves, Soldiers, Commoners, Nobles] = extract_castes(Population),   

    starve_caste(Nobles, 
    starve_caste(Commoners,
    starve_caste(Soldiers, 
    starve_caste(Slaves, InsufficientFood)))).

extract_castes(Population) ->
    % Caste is the 5th element of the population record
    lists:keysort(5, Population).

starve_caste(_Caste, 0) ->
    0;

starve_caste(Caste, InsufficientFood) ->
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
    NewInsufficientFood.

get_available_pop(CityId, Caste) ->
    log4erl:info("GetAvailablePop CityId: ~w Caste: ~w",[CityId, Caste]),
    Assignments = db:dirty_match_object({assignment, '_', CityId, Caste, '_', '_', '_'}),
    log4erl:info("Assignments: ~w", [Assignments]),
    
    [TotalPop] = db:dirty_read(population, {CityId, Caste}),
    TotalAssignment = total_assignment(Assignments, 0),
    TotalPop#population.value - TotalAssignment.   

total_assignment([], TotalAssignment) ->
    TotalAssignment;

total_assignment([Assignment | Rest], TotalAssignment) ->
    NewTotalAssignment = Assignment#assignment.amount + TotalAssignment,
    total_assignment(Rest, NewTotalAssignment).

is_valid_task(CityId, ImprovementId, ?TASK_IMPROVEMENT) ->
    case db:dirty_read(improvement, ImprovementId) of
        [Improvement] ->
            case Improvement#improvement.city_id =:= CityId of
                true ->
                    Result = true;
                false ->
                    Result = false
            end;
        _ ->
            Result = false
    end,
    Result;
is_valid_task(CityId, BuildingId, ?TASK_BUILDING) ->
    case db:dirty_read(building, BuildingId) of
        [Building] ->
            case Building#building.city_id =:= CityId of
                true ->
                    Result = true;
                false ->
                    Result = false
            end;
        _ ->
            Result = false
    end,
    Result;
is_valid_task(CityId, BuildingQueueId, ?TASK_CONSTRUCTION) ->
    case db:dirty_read(building_queue, BuildingQueueId) of
        [BuildingQueue] ->
            case BuildingQueue#building_queue.city_id =:= CityId of
                true ->
                    Result = true;
                false ->
                    Result = false
            end;
        _->
            Result = false
    end,
    Result;
is_valid_task(_CityId, _TaskId, _TaskType) ->
    false.

process_assignment(_City, Caste, Amount, BuildingQueueId, ?TASK_CONSTRUCTION) ->
    log4erl:info("~w: Process Assignment", [?MODULE]),

    case db:dirty_read(building_queue, BuildingQueueId) of
        [BuildingQueue] ->
            CurrentTime = util:get_time_seconds(),       
            StartTime = BuildingQueue#building_queue.start_time,
            EndTime = BuildingQueue#building_queue.end_time,
            CompletionRate = CurrentTime / (EndTime - StartTime),

            NewBuildingTime = building:calc_building_time(BuildingQueue#building_queue.building_type,
                                                          CompletionRate,
                                                          Caste,
                                                          Amount),
            StartTime = BuildingQueue#building_queue.start_time,
            NewBuildingQueue = BuildingQueue#building_queue { end_time = StartTime + NewBuildingTime},
            db:dirty_write(NewBuildingQueue);
        _ ->   
            log4erl:error("~w: Invalid BuildingQueueId ~w", [?MODULE, BuildingQueueId]),
            erlang:error("Invalid BuildingQueueId")
    end;          

process_assignment(_City, _Caste, _Amount, _TaskId, _TaskType) ->
    do_nothing.     

store_assigned_task(CityId, Caste, Amount, TaskId, TaskType) ->
    AssignmentId = counter:increment(assignment),
    Assignment = #assignment {  id = AssignmentId,
                                city_id = CityId,
                                caste = Caste,
                                amount = Amount,
                                task_id = TaskId,
                                task_type = TaskType },
    db:dirty_write(Assignment),
    AssignmentId.
   
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

claims_tuple(Claims) ->
    F = fun(Claim, ClaimList) ->
            ClaimTuple = {Claim#claim.id, Claim#claim.tile_index, Claim#claim.city_id},
            [ClaimTuple | ClaimList]
        end,
    lists:foldl(F, [], Claims).

improvements_tuple(Improvements) ->
    F = fun(Improvement, ImprovementList) ->
            ImprovementTuple = {Improvement#improvement.id, 
                                Improvement#improvement.type},
            [ImprovementTuple | ImprovementList]
        end,
    lists:foldl(F, [], Improvements).

assignments_tuple(Assignments) ->
    F = fun(Assignment, AssignmentList) ->
            AssignmentTuple = {Assignment#assignment.id,
                               Assignment#assignment.caste,
                               Assignment#assignment.amount,
                               Assignment#assignment.task_id,
                               Assignment#assignment.task_type},
            [AssignmentTuple | AssignmentList]
        end,
    lists:foldl(F, [], Assignments).

populations_tuple(Populations) ->
    F = fun(Population, PopulationList) ->
        PopulationTuple = {Population#population.city_id,
                           Population#population.caste,
                           erlang:trunc(Population#population.value)},
        [PopulationTuple | PopulationList]
    end,
    lists:foldl(F, [], Populations).

save_city(City) ->
    db:dirty_write(City).
