-record(counter, {type, 
                  value}).

-record(player, {id,
                 name,
                 password,
                 login_errors = 0,
                 disabled = false}).

-record(connection, {player_id,
                     process = none,
                     socket = none}).

-record(tile, {index,
               type,
               resources}).

-record(resource, {id,
                   type,
                   total,
                   regen_rate,
                   last_update}).

-record(kingdom, {id, 
                 player_id,
                 name,
                 gold = 0,
                 armies = [],
                 cities = []}).

-record(reputation, {id,
                     player_id,
                     target_player_id,
                     value}).

-record(army, {	id,
                player_id,
                name,
                x,
                y,  
                dest,
                target,
                last_pos,
                state = 0,			
                hero = 0,
                units = gb_sets:new(),
                battle}).

-record(hero, {id,
               army_id,
               level}).

-record(unit, {id,
               entity_id,
               entity_type,
               type,
               size,
               hp}).

-record(city, { id,
                player_id,
                name,
                x,
                y, 
                state = 0,
                buildings = [],
                units = [],
                claims = [],
                last_update_time}).

-record(battle, {id,
                 armies = [],
                 observed_by = [],
                 x,
                 y}).

-record(improvement, {id,
                      tile_index,
                      player_id,
                      city_id,
                      type,
                      state,
                      hp,
                      observed_by}).

-record(building, {id,
                   city_id,
                   type,
                   hp,
                   state}).

-record(population, {ref, % ref = {city_id, caste, race} %
                     city_id,
                     caste,
                     race,
                     value}).                     

-record(claim, {id, 
        		tile_index,
		        city_id,
                army_id,
                state,
                created_time}).

-record(transport, {id,
                    player_id,
                    units}).

-record(item, {id, 
               ref, %% ref = {EntityId, PlayerId}
               type,
               volume}).                   

-record(market_item, {id,
                      ref, %% ref = {EntityId, PlayerId}
                      type,
                      volume}). 

-record(assignment, {id,
                     city_id,
                     caste,
                     race,
                     amount,
                     target_ref %% target_id, target_type
                     }).

-record(trade_route, {city_id,
                      cities = []}).

-record(market_order, {id,
                       city_id,
                       player_id,
                       item_id,
                       item_type,
                       item_volume,
                       price,
                       type,
                       start_time,
                       duration}).

%%% Reference tables %%%
-record(item_type_ref, {entity_type_ref,  %% ref = {EntityId, PlayerId, Type} %%
                       item_id}).

-record(player_type, {player_id,
                      type}).

-record(reputation_ref, {ref, %% ref = {PlayerId, TargetPlayerId}
                         reputation_id}). 


%%% Queue tables %%%

-record(contract, {id,
                   city_id,
                   type, %% contract type
                   target_ref, %% {target_id, target_type}
                   object_type,
                   production,
                   created_time,
                   last_update}).

-record(unit_queue, {contract_id,                     
                     unit_type,
                     unit_size}).

-record(building_queue, {contract_id,
                         building_id,
                         building_type
                         }).

-record(improvement_queue, {contract_id,
                            improvement_id,
                            improvement_type}).

-record(item_queue, {contract_id,
                     player_id,
                     item_type,
                     item_size}).

%%% Type tables %%%
-record(unit_type, {id,
                    name,
                    level,
                    attack,
                    defense,
                    speed,
                    total_hp,
                    movement,
                    production_cost,
                    gold_cost
                    }).

-record(population_type, {id,
                          name}).

-record(resource_type, {id,
                        tile_max,
                        name}).

-record(improvement_type, {id,
                           name,
                           total_hp,
                           production_cost}).

-record(building_type, {id,
                        name,
                        total_hp,
                        production_cost,
                        gold_cost}).

-record(item_type, {id,
                    name,
                    production_cost,
                    structure_req = {}}).







