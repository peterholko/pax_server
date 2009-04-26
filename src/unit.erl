%% Author: Peter
%% Created: Mar 1, 2009
%% Description: TODO: Add description to unit
-module(unit).

%%
%% Include files
%%

-include("common.hrl").
-include("schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

%%
%% Exported Functions
%%
-export([transfer/4, units_tuple/1, units_queue_tuple/1, db_units_info/1]).

%%
%% API Functions
%%
transfer(EntityId, UnitId, TargetId, TargetAtom) ->
  
    case db:read(unit, UnitId) of
        [Unit] ->
            if
                EntityId =:= Unit#unit.entity_id ->
					TargetPlayerId = gen_server:call(global:whereis_name({TargetAtom, TargetId}), {'GET_ID'}),
                    db_transfer_unit(Unit, TargetId),
                	TransferUnitInfo = {unit_transfer, success};
                true ->
                    TransferUnitInfo = {unit_transfer, incorrect_army}
            end;
        _ ->
            TransferUnitInfo = {unit_transfer, no_unit}
    end,

	TransferUnitInfo.


units_tuple([]) ->
    [];

units_tuple(Units) ->
    F = fun(Unit, UnitList) ->
                UnitTuple = {Unit#unit.id, Unit#unit.type, Unit#unit.size},
                [UnitTuple | UnitList]
        end,
    
    lists:foldl(F, [], Units).

units_queue_tuple([]) ->
    [];
units_queue_tuple(UnitsQueue) ->
    
    F = fun(UnitQueue, UnitQueueList) ->
                UnitQueueTuple = {UnitQueue#unit_queue.id, 
                                  UnitQueue#unit_queue.unit_type, 
                                  UnitQueue#unit_queue.unit_size,
                                  UnitQueue#unit_queue.start_time,
                                  UnitQueue#unit_queue.end_time},
                [UnitQueueTuple | UnitQueueList]
        end,
    
    lists:foldl(F, [], UnitsQueue).

db_transfer_unit(Unit, TargetId) ->
    
    F = fun() ->                
			NewUnit = Unit#unit{entity_id = TargetId},
            mnesia:write(NewUnit)
        end,

	mnesia:transaction(F). 

db_units_info(EntityId) ->
	db:index_read(unit, EntityId, #unit.entity_id).
