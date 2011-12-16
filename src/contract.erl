%% Author: Peter
%% Created: March 5, 2011
%% Description: TODO: Add description to object
-module(contract).

%%
%% Include files
%%

-include("common.hrl").
-include("schema.hrl").
%%
%% Exported Functions
%%
-export([exists/3,
         tuple_form/1,
         process_contracts/1]).
%%
%% API Functions
%%

exists(CityId, TargetId, TargetType) ->
    Contracts = db:dirty_index_read(contract, CityId, #contract.city_id),
    case lists:keyfind({TargetId, TargetType}, #contract.target_ref, Contracts) of
        false ->
            Result = false;
        _Contract ->
            Result = true
    end,
    Result.
                
process_contracts(Contracts) ->
    check_contracts(Contracts).

tuple_form(Contracts) ->
    F = fun(Contract, ContractList) ->
            {TargetId, TargetType} = Contract#contract.target_ref,

            ContractTuple = {Contract#contract.id,
                             Contract#contract.city_id,
                             Contract#contract.type,
                             TargetType,
                             TargetId,
                             Contract#contract.object_type,
                             trunc(Contract#contract.production),
                             Contract#contract.created_time,
                             Contract#contract.last_update},
            [ContractTuple | ContractList]
        end,
    lists:foldl(F, [], Contracts).
%%
%% Local Functions
%%

check_contracts([]) ->
    done;

check_contracts([Contract | Rest]) ->
    log4erl:info("{~w} Checking contract: ~w", [?MODULE, Contract]),
    Assignments = db:dirty_index_read(assignment, Contract#contract.target_ref, #assignment.target_ref),

    CurrentTime = util:get_time_seconds(),
    LastUpdateTime = Contract#contract.last_update,
    NumGameDays = util:diff_game_days(LastUpdateTime, CurrentTime),

    case process_assignments(Contract, Assignments, NumGameDays, incomplete) of
        {NewContract, complete} ->
	    db:dirty_delete(contract, NewContract#contract.id);
	{NewContract, incomplete} ->
	    UpdatedContract = NewContract#contract { last_update = CurrentTime},
            db:dirty_write(UpdatedContract)
    end,

    check_contracts(Rest).

process_assignments(NewContract, [], _NumGameDays, NewContractStatus) ->
    {NewContract, NewContractStatus};

process_assignments(NewContract, _Assignments, _NumGameDays, complete) ->
    {NewContract, complete};

process_assignments(Contract, [Assignment | Rest], NumGameDays, incomplete) ->
    log4erl:info("{~w} Contract and Assignment matched: ~w == ~w", [?MODULE, Contract, Assignment]),
    ContractType = Contract#contract.type, 
    {NewContract, NewContractStatus} = process_type(ContractType, Contract, Assignment, NumGameDays),

    process_assignments(NewContract, Rest, NumGameDays, NewContractStatus).
    
process_type(?CONTRACT_BUILDING, Contract, Assignment, NumGameDays) ->
    log4erl:info("{~w} - Process CONTRACT_BUILDING type",[?MODULE]),

    {TargetId, _TargetType} = Assignment#assignment.target_ref,
    BuildingId = TargetId,
    [Building] = db:dirty_read(building, BuildingId),
    [BuildingType] = db:dirty_read(building_type, Building#building.type),

    ProductionCost = BuildingType#building_type.production_cost,
    TotalHp = BuildingType#building_type.total_hp,

    Result = process_production(Contract, Assignment, NumGameDays, ProductionCost),

    case Result of
        {_NewContract, complete} ->                       
            NewBuilding = Building#building {hp = TotalHp,
                                             state = ?STATE_CONSTRUCTING },
            db:dirty_write(NewBuilding);
        {NewContract, incomplete} ->
            CompletionRatio = NewContract#contract.production / ProductionCost,
            NewBuildingHp = util:round3(TotalHp * CompletionRatio),
            NewBuilding = Building#building {hp = NewBuildingHp},

            db:dirty_write(NewBuilding)
    end,
    Result;

process_type(?CONTRACT_UNIT, Contract, Assignment, NumGameDays) ->
    log4erl:info("{~w} - Process CONTRACT_UNIT type",[?MODULE]),
    
    [UnitQueue] = db:dirty_read(unit_queue, Contract#contract.id),
    [UnitType] = db:dirty_read(unit_type, UnitQueue#unit_queue.unit_type),

    ProductionCost = UnitType#unit_type.production_cost,

    Result = process_production(Contract, Assignment, ProductionCost, NumGameDays),
    case Result of
        {NewContract, complete} ->

            Unit = #unit {id = counter:increment(unit),
                          entity_id = NewContract#contract.city_id,
                          entity_type = ?OBJECT_CITY,
                          type = UnitQueue#unit_queue.unit_type,
                          size = UnitQueue#unit_queue.unit_size,
                          hp = UnitType#unit_type.total_hp},

            db:dirty_write(Unit),
            db:dirty_delete(unit_queue, NewContract#contract.id);
        {_NewContract, incomplete} ->
            ok
    end,
    Result;

process_type(?CONTRACT_IMPROVEMENT, Contract, Assignment, NumGameDays) ->
    log4erl:info("{~w} - Process CONTRACT_IMPROVEMENT",[?MODULE]),

    TypeId = Contract#contract.object_type,
    [ImprovementType] = db:dirty_read(improvement_type, TypeId),
    
    ProductionCost = ImprovementType#improvement_type.production_cost,
    TotalHp = ImprovementType#improvement_type.total_hp,
    {TargetId, _TargetType} = Assignment#assignment.target_ref,
    ImprovementId = TargetId,

    Result = process_production(Contract, Assignment, ProductionCost, NumGameDays),
    case Result of
        {_NewContract, complete} ->
            improvement:complete(ImprovementId);
        {NewContract, incomplete} ->
            CompletionRatio = NewContract#contract.production / ProductionCost,
            improvement:update_hp(ImprovementId, TotalHp, CompletionRatio)
    end,
    Result;

process_type(?CONTRACT_HARVEST, Contract, Assignment, NumGameDays) ->
    log4erl:info("{~w} - Process CONTRACT_HARVEST",[?MODULE]),
    [ItemQueue] = db:dirty_read(item_queue, Contract#contract.id),
    [ItemType] = db:dirty_read(item_type, ItemQueue#item_queue.item_type),

    ProductionCost = ItemType#item_type.production_cost,
    {TargetId, _TargetType} = Assignment#assignment.target_ref,

    Result = process_production(Contract, Assignment, ProductionCost, NumGameDays),
    case Result of 
        {NewContract, complete} ->
            {TargetId, _TargetType} = Assignment#assignment.target_ref,
            ImprovementId = TargetId,
            [Improvement] = db:dirty_read(improvement, ImprovementId),            

            ResourceGained = map:harvest_resource(Improvement#improvement.tile_index,
                                                  ItemType#item_type.id,
                                                  ItemQueue#item_queue.item_size),

            F = fun(ItemTypeId) ->
                    item:create(NewContract#contract.city_id,
                                ItemQueue#item_queue.player_id,
                                ItemTypeId,
                                ResourceGained)
                end,

            lists:foreach(F, ItemType#item_type.produces),

            db:dirty_delete(item_queue, NewContract#contract.id);
        {_NewContract, incomplete} ->
            ok
    end,
    Result;
  
process_type(?CONTRACT_ITEM, Contract, Assignment, NumGameDays) ->
    log4erl:info("{~w} - Process CONTRACT_ITEM type", [?MODULE]),
    
    [ItemQueue] = db:dirty_read(item_queue, Contract#contract.id),
    [ItemType] = db:dirty_read(item_type, ItemQueue#item_queue.item_type),
    
    ProductionCost = ItemType#item_type.production_cost,

    Result = process_production(Contract, Assignment, ProductionCost, NumGameDays),
    case Result of
        {NewContract, complete} ->

            item:create(NewContract#contract.city_id,
                        ItemQueue#item_queue.player_id,
                        ItemQueue#item_queue.item_type,
                        ItemQueue#item_queue.item_size),

            db:dirty_delete(item_queue, NewContract#contract.id);
        {_NewContract, incomplete} ->
            ok
    end,
    Result;

process_type(ContractType, _Contract, _Assignment, _NumGameDays) ->
    log4erl:info("{~w} Invalid Contract Type: ~w", [?MODULE, ContractType]).
                        
process_production(Contract, Assignment, ProductionCost, NumGameDays) ->
    log4erl:info("{~w} Process Production", [?MODULE]),
    
    Production = Contract#contract.production,
    CasteRate = caste:get_production_rate(Assignment#assignment.caste),
    CasteAmount = Assignment#assignment.amount,
    NewProduction = Production + (CasteRate * NumGameDays * CasteAmount),
    log4erl:info("Diff: ~w Production: ~w Caste: ~w NumGameDays: ~w CasteAmount: ~w", [(NewProduction - Production), Production, CasteRate, NumGameDays * 1000000, CasteAmount]),
    case NewProduction >= ProductionCost of
        true ->
            log4erl:info("{~w} Contract complete", [?MODULE]),
            NewContract = Contract,
            ProductionStatus = complete;
        false ->
            log4erl:info("{~w} Updating Production: ~w", [?MODULE, NewProduction]),
            RoundedProduction = util:round3(NewProduction),
            NewContract = Contract#contract {production = RoundedProduction},            
            ProductionStatus = incomplete
    end,

    {NewContract, ProductionStatus}. 

