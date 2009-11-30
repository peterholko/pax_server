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

-export([start/2, stop/1, queue_unit/4, add_claim/3, add_improvement/4]).

-record(module_data, {city,                       
                      units_queue,
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
    UnitQueue = db:index_read(unit_queue, City#city.id, #unit_queue.city_id),

    %Inventory setup    
    Inventory = dict:new(),

    {ok, #module_data{city = City, 
                      units_queue = UnitQueue,
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

handle_cast({'ADD_UNIT', Unit}, Data) ->
    
    Units = Data#module_data.units,
    NewUnits = [Unit | Units],
    NewData = Data#module_data {units = NewUnits},
    
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
    
    case EventType of
        ?EVENT_HARVEST -> 
            log4erl:info("Processing Harvest for City ~w~n", [self()]),
            NewInventory = harvest(Data#module_data.improvements, Data#module_data.inventory),
            NewData = Data#module_data {inventory = NewInventory} 
    end,      
    
    {noreply, NewData};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({'QUEUE_UNIT', PlayerId, UnitType, UnitSize}, _From, Data) ->
    
    City = Data#module_data.city,
    UnitsQueue = Data#module_data.units_queue,
    
    if
        City#city.player_id =:= PlayerId ->
            NewUnitsQueue = add_unit_to_queue(City#city.id, UnitsQueue, UnitType, UnitSize),
            NewData = Data#module_data { units_queue = NewUnitsQueue},
            Result = {city, queued_unit};
        true ->
            NewData = Data,
            Result = {city, error}
    end,
    
    {reply, Result, NewData};

handle_call({'GET_INFO', PlayerId}, _From, Data) ->
    
    City = Data#module_data.city,
    
    if 
        City#city.player_id =:= PlayerId ->
            
            %Get UnitsQueue and Units
            UnitsQueue = Data#module_data.units_queue,
            UnitIds = City#city.units,
            
            %Check if any units are completed
            {NewUnitsQueue, UnitsToAdd} = check_queue(UnitsQueue),
            
            %Add any units from the queue
            NewUnitIds = add_units_from_queue(UnitIds, UnitsToAdd),
            
            %Assign any changes to module data
            NewCity = City#city {units = NewUnitIds}, 
            NewData = Data#module_data {city = NewCity, units_queue = NewUnitsQueue},            
                        
            %Convert record to tuple packet form
            [NewUnits] = db:dirty_index_read(unit, City#city.id, #unit.entity_id),
            UnitsTuple = unit:units_tuple(NewUnits),
            UnitsQueueTuple = unit:units_queue_tuple(NewUnitsQueue),
            
            CityInfo = {detailed, City#city.buildings, UnitsTuple, UnitsQueueTuple};
        true ->
            NewData = Data,
            CityInfo = {generic, City#city.player_id}
    end,
    
    io:fwrite("city - CityInfo: ~w~n", [CityInfo]),
    {reply, CityInfo , NewData};

handle_call({'TRANSFER_UNIT', _SourceId, UnitId, TargetId, TargetAtom}, _From, Data) ->
    
    io:fwrite("city - transfer unit.~n"),
    
    Units = Data#module_data.units,
    UnitResult = dict:is_key(UnitId, Units),
    
    if
        UnitResult =/= false ->
            Unit = dict:fetch(UnitId, Units),
            
            TargetPid = object:get_pid(TargetAtom, TargetId),

            case gen_server:call(TargetPid, {'RECEIVE_UNIT', TargetId, Unit, Data#module_data.player_id}) of
                {receive_unit, success} ->
                    NewUnits = dict:erase(UnitId, Units),
                    NewData = Data#module_data {units = NewUnits, save_city = true},
                    TransferUnitInfo = {transfer_unit, success};
                Error ->
                    TransferUnitInfo = Error,
                    NewData = Data
            end;
        true ->
            TransferUnitInfo = {transfer_unit, error},
            NewData = Data
    end,         		
    
    {reply, TransferUnitInfo , NewData};

handle_call({'RECEIVE_UNIT', _TargetId, Unit, PlayerId}, _From, Data) ->
    
    io:fwrite("city - receive unit.~n"),
    
    City = Data#module_data.city,
    Units = Data#module_data.units,
    
    if
        PlayerId =:= Data#module_data.player_id ->
            %Check to see if unit already exists
            UnitResult = dict:is_key(Unit#unit.id, Units),
            
            if
                UnitResult =:= false ->
                    
                    NewUnit = Unit#unit {entity_id = City#city.id},
                    NewUnits = dict:store(Unit#unit.id, NewUnit, Units),
                    NewData = Data#module_data {units = NewUnits, save_city = true},
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

check_queue([]) ->
    {[], []};

check_queue(QueueInfo) ->
    CurrentTime = util:get_time_seconds(),
    
    io:fwrite("city - check_queue - CurrentTime: ~w~n", [CurrentTime]),  
    
    F = fun(UnitQueue, UnitsToRemove) ->
                EndTime = UnitQueue#unit_queue.end_time,
                
                if
                    EndTime =< CurrentTime ->    
                        io:fwrite("city - check_queue - UnitQueue: ~w~n", [UnitQueue]),    
                        NewUnitsToRemove = [UnitQueue | UnitsToRemove];
                    true ->
                        NewUnitsToRemove = UnitsToRemove
                end,
                
                NewUnitsToRemove
        end,
    
    UnitsToRemove = lists:foldl(F, [], QueueInfo),
    
    NewQueueInfo = QueueInfo -- UnitsToRemove,
    io:fwrite("city - check_queue - UnitsRemoved: ~w~n", [UnitsToRemove]),    
    {NewQueueInfo, UnitsToRemove}.  

add_units_from_queue(Units, []) ->
    Units;

add_units_from_queue(Units, UnitsToAdd)->
    
    [UnitQueue | Rest] = UnitsToAdd,
    
    Unit = #unit {id = counter:increment(unit),
                  entity_id = UnitQueue#unit_queue.city_id,
                  entity_type = ?OBJECT_CITY,
                  type = UnitQueue#unit_queue.unit_type,
                  size = UnitQueue#unit_queue.unit_size},
    
    db:dirty_write(unit, Unit),
    NewUnits = [Unit#unit.id | Units]        
    add_units_from_queue(NewUnits, Rest).

%Valid X, Valid Y, Exists, MaxReached
check_claim(true, true, false, false) ->
    true;
check_claim(_, _, _, _) ->
    log4erl:info("Add claim failed."),
    false.

add_item(Type, Amount, Inventory) ->
    
    case dict:is_key(Type, Inventory) of
        true ->
            CurrentAmount = dict:fetch(Type, Inventory),
            NewInventory = dict:store(Type, CurrentAmount + Amount, Inventory);
        false ->
            NewInventory = dict:store(Type, Amount, Inventory)
    end,
    
    NewInventory.

harvest([], Inventory) ->
    Inventory;

harvest([ImprovementId | Rest], Inventory) ->
    [Improvement] = db:dirty_read(improvement, ImprovementId),
    [_NumResources | Resources] = map_port:get_resources(Improvement#improvement.tile_index),

    Yield = get_yield(Improvement#improvement.type, Resources, undefined, false),
    ResourceGained = Yield * 1,

    io:fwrite("ResourceGained: ~w~n",[ResourceGained]),

    NewInventory = add_item(Improvement#improvement.type, ResourceGained, Inventory),
    harvest(Rest, NewInventory).

get_yield(_, [], _, false) ->
    0;
get_yield(_, [], Yield, true) ->
    Yield;
get_yield(SearchType, Resources, Yield, Result) ->
    [Type | Rest] = Resources,
    [NewYield | NewResources] = Rest,
    NewResult = Type =:= SearchType,
    get_yield(SearchType, NewResources, NewYield, NewResult).


    


