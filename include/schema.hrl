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

-record(entity, {id, 
                 player_id,
                 type}).

-record(army, {	id,
                player_id,
                name,
                x,
                y,  
                dest = [],
                target = none,
                last_pos = none,
                state = 0,			
                hero = 0,
                units = gb_sets:new(),
                battle = none}).

-record(hero, {id,
               army_id,
               level}).

-record(unit, {id,
               entity_id,
               recipe_id,
               template_id,
               size,
               current_hp,
               name,
               gear = []}).

-record(city, { id,
                player_id,
                name,
                x,
                y, 
                state = 0,
                buildings = [],
                units = [],
                claims = [],
                tax_commoner,
                tax_noble,
                tax_tariff,
                loyalty,
                health, 
                law, 
                last_update_time}).

-record(battle, {id,
                 players = sets:new(),
                 armies = [],
                 observed_by = [],
                 x,
                 y,
                 targets = [],
                 events = [],
                 last_events = sets:new(),
                 soldiers = [],
                 killed = [],
                 reserve = []}).

-record(map_object, {id,
                     type,
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
               ref, %% ref = {OwnerRef, PlayerId}
               type, %% item_base
               template_id,
               volume}).                   

-record(market_item, {id,
                      ref, %% ref = {OwnerRef, PlayerId}
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
-record(item_type_ref, {owner_type_ref,  %% ref = {EntityId, PlayerId, Type} %%
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
                     unit_size,
                     gear}).

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
%-record(unit_type, {id,
%                    name,
%                    level,
%                    attack,
%                    defense,
%                    speed,
%                    total_hp,
%                    movement,
%                    production_cost,
%                    gold_cost
%                    }).

-record(population_type, {id,
                          name}).

-record(improvement_type, {id,
                           category,
                           level,
                           name,
                           total_hp,
                           population_cap,
                           production_cost,
                           gold_cost,
                           lumber_cost,
                           stone_cost,
                           upkeep}).

-record(resource_type, {id, 
                        name}).

-record(building_type, {id,
                        building_type,
                        level,
                        name,
                        hp,
                        population_cap,
                        production_cost,
                        gold_cost,
                        lumber_cost,
                        stone_cost,
                        upkeep}).

-record(item_base, {type_id,
                    name,
                    category,
                    production_cost,
                    batch_amount,
                    building_req,
                    improvement_req,
                    produces = [],
                    material_amount,
                    material_type}).

-record(item_recipe, {type_id,
                      template_id,
                      player_id,
                      item_name,
                      flavour_text,
                      material_amount = [],
                      material_type = []}).

-record(item_category, {id, 
                        name,
                        display_name,
                        contains}).

-record(item_template, {template_id,
                        category,
                        name,
                        production_cost,
                        batch_amount,
                        building_req,
                        material_amount = [],
                        material_category = []}).

-record(unit_recipe, {id,
                      template_id,
                      player_id,
                      unit_name,
                      default_size,
                      gear = []}).

-record(unit_template, {id,
                    name,
                    level,
                    type,
                    building_req,
                    hp,
                    atk,
                    def,
                    range,
                    speed,
                    acc,
                    eva,
                    effect_amount,
                    effact_type,
                    production_cost,
                    gold_cost,
                    material_amount,
                    material_type,
                    upkeep,
                    food,
                    capacity,
                    def_size,
                    min_size,
                    max_size,
                    movement,
                    value}).
