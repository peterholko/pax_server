%% Author: Peter
%% Created: Mar 17, 2009
%% Description: TODO: Add description to building
-module(building).

%%
%% Include files
%%

-include("common.hrl").
-include("schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

%%
%% Exported Functions
%%
-export([get_info/2]).

%%
%% API Functions
%%

get_info(BuildingId, PlayerId) ->

    case db:read(building_type, BuildingId) of
       [BuildingInfo] ->
               case BuildingInfo#building_type.type of
                   ?BUILDING_UNIT_LAND ->
                       QueueInfo = get_queue_info(PlayerId, BuildingInfo#building_type.type);
                   ?BUILDING_UNIT_SEA ->
                       QueueInfo = get_queue_info(PlayerId, BuildingInfo#building_type.type);
                   ?BUILDING_UNIT_AIR ->
                       QueueInfo = get_queue_info(PlayerId, BuildingInfo#building_type.type);
                   _ ->
                       QueueInfo = {none}
               end;
        _ ->
            QueueInfo = {none}
    end,
    
    QueueInfo.                          

%%
%% Local Functions
%%
get_queue_info(PlayerId, BuildingType) ->
	QueueUnits = queue_units(PlayerId, BuildingType),
   	{unit_queue, BuildingType, QueueUnits}.
            
queue_units(PlayerId, BuildingType) ->	
	db:do(qlc:q([{X#unit_queue.unit_type, X#unit_queue.unit_amount, 
                  X#unit_queue.start_time, X#unit_queue.build_time} || X <- mnesia:table(unit_queue),
                                                                       X#unit_queue.player_id =:= PlayerId,
                                                                       X#unit_queue.building_type =:= BuildingType])).
  
