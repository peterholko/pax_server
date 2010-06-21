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

-export([start/2, stop/1, queue_unit/4, queue_building/3, add_claim/3, add_improvement/4]).

-record(module_data, {city,                       
                      units_queue,
                      buildings_queue,
                      improvements = [],
                      inventory,
                      player_id, 
                      self,
                      visible = [],
                      observed_by = [],
                      save_city = false}).
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
    BuildingsQueue = db:index_read(building_queue, City#city.id, #building_queue.city_id),

    %Inventory setup    
    Inventory = dict:new(),

    {ok, #module_data{city = City, 
                      units_queue = UnitsQueue,
                      buildings_queue = BuildingsQueue,
                      inventory = Inventory, 
                      player_id = PlayerId, 
                      self = self() }}.

terminate(_Reason, _) ->
    ok.

stop(ProcessId) 
  when is_pid(ProcessId) ->
    gen_server:cast(ProcessId, stop).

queue_unit(CityId, PlayerId, UnitType, UnitSize) ->
    gen_server:call(global:whereis_name({city, CityId}), {'QUEUE_UNIT', PlayerId, UnitType, UnitSize}).

queue_building(CityId, PlayerId, BuildingType) ->
    gen_server:call(global:whereis_name({city, CityId}), {'QUEUE_BUILDING', PlayerId, BuildingType}).

add_improvement(CityId, X, Y, ImprovementType) ->
    gen_server:cast(global:whereis_name({city,CityId}), {'ADD_IMPROVEMENT', X, Y, ImprovementType}).

add_claim(CityId, X, Y) ->
    gen_server:cast(global:whereis_name({city, CityId}), {'ADD_CLAIM', X, Y}).
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
    
    {noreply, NewData};

handle_cast({'ADD_IMPROVEMENT', X, Y, ImprovementType}, Data) ->
    City = Data#module_data.city,

    case lists:member({X,Y}, City#city.claims) of
        true ->
            ImprovementPid = global:whereis_name(improve_pid),
            ImprovementId = counter:increment(improvement),
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

handle_cast({'ADD_CLAIM', X, Y}, Data) ->

    City = Data#module_data.city,
    TileIndex = map:convert_coords(X, Y),
       
    ValidX = abs(City#city.x - X) =< 6,
    ValidY = abs(City#city.y - Y) =< 6,
    Exists = length(db:index_read(claim, TileIndex, #claim.tile_index)) > 0,
    MaxReached = length(City#city.claims) > 3,

    io:fwrite("ValidX: ~w ValidY: ~w Exists: ~w MaxReached: ~w~n", [ValidX, ValidY, Exists, MaxReached]), 

    case check_claim(ValidX, ValidY, Exists, MaxReached) of
        true ->
            log4erl:info("Add claim successful."),
            NewClaimRecord = #claim {id = counter:increment(claim),
                                     tile_index = TileIndex,
                                     city_id = City#city.id},
            Result = db:write(NewClaimRecord),
            io:fwrite("Add Claim - result: ~w~n", [Result]),

            NewClaims = [{X, Y} | City#city.claims],
            NewCity = City#city { claims = NewClaims },
            NewData = Data#module_data { city = NewCity };
        false ->
            NewData = Data
    end,

    {noreply, NewData};

handle_cast({'PROCESS_EVENT', _Id, EventType}, Data) ->   
    City = Data#module_data.city,

    case EventType of
        ?EVENT_HARVEST -> 
            %log4erl:info("Processing Harvest for City ~w~n", [self()]),
            harvest(Data#module_data.improvements, City#city.id);
        ?EVENT_GROWTH ->
            growth(City#city.id)            
    end,      
    
    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({'QUEUE_UNIT', PlayerId, UnitType, UnitSize}, _From, Data) ->   
    City = Data#module_data.city,
    UnitsQueue = Data#module_data.units_queue,
    
    case City#city.player_id =:= PlayerId of
        true ->              
            NewUnitsQueue = add_unit_to_queue(City#city.id, UnitsQueue, UnitType, UnitSize),
            NewData = Data#module_data { units_queue = NewUnitsQueue},
            Result = {city, queued_unit};
        false ->
            NewData = Data,
            Result = {city, error}
    end,
    
    {reply, Result, NewData};

handle_call({'QUEUE_BUILDING', PlayerId, BuildingType}, _From, Data) ->
    City = Data#module_data.city,
    BuildingsQueue = Data#module_data.buildings_queue,

    case City#city.player_id =:= PlayerId of
        true ->
            NewBuildingsQueue = add_building_to_queue(City#city.id, BuildingsQueue, BuildingType),
            NewData = Data#module_data { buildings_queue = NewBuildingsQueue},
            Result = {city, queued_building};
        false ->
            NewData = Data,
            Result = {city, error}
    end,

    {reply, Result, NewData}; 

handle_call({'GET_INFO', PlayerId}, _From, Data) ->    
    City = Data#module_data.city,
    
    if 
        City#city.player_id =:= PlayerId ->           
            %Get UnitQueue and Units
            UnitsQueue = Data#module_data.units_queue,
            UnitIds = City#city.units,

            %Get BuildingQueue and buildings,
            BuildingsQueue = Data#module_data.buildings_queue,
            BuildingIds = City#city.buildings,
            
            %Check if any units or buildings are completed
            {NewUnitsQueue, UnitsToAdd} = check_unit_queue(UnitsQueue),
            {NewBuildingsQueue, BuildingsToAdd} = check_building_queue(BuildingsQueue),
            
            %Add any units from the queue
            NewUnitIds = add_units_from_queue(UnitIds, UnitsToAdd),
            NewBuildingIds = add_buildings_from_queue(BuildingIds, BuildingsToAdd),
            
            %Assign any changes to module data
            NewCity = City#city {units = NewUnitIds, buildings = NewBuildingIds}, 
            NewData = Data#module_data {city = NewCity, 
                                        units_queue = NewUnitsQueue, 
                                        buildings_queue = NewBuildingsQueue},            
                        
            %Convert units record to tuple packet form
            NewUnits = db:dirty_index_read(unit, City#city.id, #unit.entity_id),
            UnitsTuple = unit:units_tuple(NewUnits),
            UnitsQueueTuple = unit:units_queue_tuple(NewUnitsQueue),

            %Convert buildings record to tuple packet form
            NewBuildings = db:dirty_index_read(building, City#city.id, #building.city_id),  
            BuildingsTuple = building:buildings_tuple(NewBuildings),
            BuildingsQueueTuple = building:buildings_queue_tuple(NewBuildingsQueue),        
            
            CityInfo = {detailed, BuildingsTuple, BuildingsQueueTuple, UnitsTuple, UnitsQueueTuple};
        true ->
            NewData = Data,
            CityInfo = {generic, City#city.player_id}
    end,
    
    io:fwrite("city - CityInfo: ~w~n", [CityInfo]),
    {reply, CityInfo , NewData};

handle_call({'TRANSFER_UNIT', _SourceId, UnitId, TargetId, TargetAtom}, _From, Data) ->   
    io:fwrite("city - transfer unit.~n"),
    City = Data#module_data.city,
    Units = City#city.units,
    
    case gb_sets:is_member(UnitId, Units) of
        true ->
            [Unit] = db:dirty_read(unit, UnitId),
            TargetPid = object:get_pid(TargetAtom, TargetId),

            case gen_server:call(TargetPid, {'RECEIVE_UNIT', TargetId, Unit, Data#module_data.player_id}) of
                {receive_unit, success} ->
                    NewUnits = gb_sets:delete(UnitId, Units),
                    NewCity = City#city {units = NewUnits},
                    NewData = Data#module_data {city = NewCity},
                    TransferUnitInfo = {transfer_unit, success};
                Error ->
                    TransferUnitInfo = Error,
                    NewData = Data
            end;
        false ->
            TransferUnitInfo = {transfer_unit, error},
            NewData = Data
    end,         		
    
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
    
    {reply, ReceiveUnitInfo, NewData};   

handle_call({'GET_STATE', _CityId}, _From, Data) ->
    City = Data#module_data.city,
    
    State = #state {id = City#city.id,
                    player_id = City#city.player_id,
                    type = ?OBJECT_CITY,
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
                                     end_time = StartTime + 60},

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
    NewBuildings = [Building#building.id | Buildings],

    add_buildings_from_queue(NewBuildings, Rest).

%Valid X, Valid Y, Exists, MaxReached
check_claim(true, true, false, false) ->
    true;
check_claim(_, _, _, _) ->
    log4erl:info("Add claim failed."),
    false.

add_item(CityId, Type, Value) ->
    case db:dirty_read(item_type_ref, {CityId, Type}) of
        [ItemTypeRef] ->
            io:fwrite("ItemTypeRef: ~w~n", [ItemTypeRef]),
            update_item(ItemTypeRef#item_type_ref.item_id, Value);
        _ ->
            new_item(CityId, Type, Value)
    end.

update_item(ItemId, Value) ->
    F = fun() ->
            [Item] = mnesia:read(item, ItemId),
            CurrentValue = Item#item.value,
            NewItem = Item#item {value = CurrentValue + Value},
            mnesia:write(NewItem)
        end,
    {atomic, _Status} = mnesia:transaction(F).

new_item(CityId, Type, Value) ->
    F = fun() ->
            ItemId = counter:increment(item),
            ItemRef = {CityId, Type},
            Item = #item {id = ItemId,
                          entity_id = CityId,
                          type = Type,
                          value = Value},
            ItemTypeRef = #item_type_ref {ref = ItemRef,
                                          item_id = ItemId},
            mnesia:write(Item),
            mnesia:write(ItemTypeRef)
        end,
    {atomic, _Status} = mnesia:transaction(F).

get_item_by_type(CityId, Type) ->
    case db:dirty_read(item_type_ref, {CityId, Type}) of
        [ItemTypeRef] ->
            [Item] = db:dirty_read(item, ItemId),
            Result = {found, Item};
        _ ->
            Result = {not_found}
    Result.            

harvest([], _) ->
    %io:fwrite("Harvest no improvements~n"),
    ok;

harvest([ImprovementId | Rest], CityId) ->
    io:fwrite("Harvest~n"),
    [Improvement] = db:dirty_read(improvement, ImprovementId),
    [_NumResources | Resources] = map_port:get_resources(Improvement#improvement.tile_index),

    Yield = get_yield(Improvement#improvement.type, Resources, undefined, false),
    ResourceGained = Yield * 1,

    io:fwrite("ResourceGained: ~w~n",[ResourceGained]),

    add_item(CityId, Improvement#improvement.type, ResourceGained),
    harvest(Rest, CityId).

get_yield(_, [], _, false) ->
    0;
get_yield(_, [], Yield, true) ->
    Yield;
get_yield(SearchType, Resources, _Yield, _Result) ->
    [Type | Rest] = Resources,
    [NewYield | NewResources] = Rest,
    NewResult = Type =:= SearchType,
    get_yield(SearchType, NewResources, NewYield, NewResult).

growth(CityId) ->
    Population = db:dirty_read(population, CityId),
    
    %Calculate food required
    TotalFoodRequired = total_food_required(Population, 0),
    
    %Subtract from food stores
    food_upkeep(CityId, TotalFoodRequired).


food_required(?CASTE_SLAVE) ->
    1;
food_required(?CASTE_SOLDIER) ->
    2;
food_required(?CASTE_COMMONER) ->
    2;
food_required(?CASTE_NOBLE) ->
    4;

growth_rate(?CASTE_SLAVE) ->
    1.03;
growth_rate(?CASTE_SOLDIER) ->
    1.02;
growth_rate(?CASTE_COMMONER) ->
    1.01;
growth_rate(?CASTE_NOBLE) ->
    1.005.

total_food_required([], TotalFoodRequired) ->
    ok;

total_food_required([Caste | Rest], FoodRequired) ->
    NewFoodRequired = food_required(Caste#population.caste),
    TotalFoodRequired = FoodRequired + NewFoodRequired,

    total_food_required(Rest, TotalFoodRequired).

food_upkeep(CityId, TotalFoodRequired) ->
    case get_item_by_type(CityId, ?ITEM_FOOD) of
        {found, Item} ->
            if
                Item#item.value >= TotalFoodRequired ->
                    add_population(Population);
                true ->
                    starve_population(Population, TotalFoodRequired - Item#item.value)
            end,
            
            update_item(Item#item.id, Item#item.value - TotalFoodRequired);
        {not_found} ->
            starve_population(Population, TotalFoodRequired)
    end.         
                                     
add_population([]) ->
    ok;

add_population([Caste | Rest]) ->
    GrowthRate = growth_rate(Caste#population.caste),
    NewValue = Caste#population.value * GrowthRate,   
    NewCaste = Caste#population {value = NewValue},

    db:write(NewCaste),

    add_population(Rest).

starve_population(Population, InsufficientFood) ->   
   
    starve_slaves(Slaves, InsufficientFood),

starve_slaves([], NewInsufficientFood) ->
    NewInsufficientFood;

starve_slaves([Caste | Rest], InsufficientFood) ->
    if
        Caste#population.caste =:= ?CASTE_SLAVES ->
            NewValue = Caste#population.value - InsufficientFood,
            NewInsufficientFood 
            NewCaste = Caste#population {value = NewValue}
            db:write(NewCaste);
        true ->
            ok
    end

