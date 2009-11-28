-record(counter, {type, 
                  value}).

-record(player, {id,
                 name,
                 password,
                 login_errors = 0,
                 disabled = false,
                 armies = [],
                 cities = []}).

-record(connection, {player_id,
                     process = none,
                     socket = none}).

-record(army, {	id,
                player_id,
                x,
                y,
                dest_x,
                dest_y,
                target,
                state = 0,			
                hero = 0,
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
                x,
                y, 
                state = 0,
                buildings = [],
                units = [],
                claims = [],
                last_update_time}).

-record(battle, {id,
                 armies = [],
                 x,
                 y}).

-record(improvement, {id,
                      tile_index,
                      player_id,
                      city_id,
                      type,
                      state,
                      observed_by}).

-record(resource, {entity_id,
                   entity_type,
                   value,
                   type}).

-record(building, {id,
                   city_id,
                   type}).

-record(population, {city_id, 
                     type,
                     value,
                     base_growth}).

-record(claim, {id, 
		tile_index,
		city_id}).

-record(transport, {id,
                    inventory,
                    units = []}).



%%% Queue tables %%%

-record(unit_queue, {id,
                     city_id,
                     unit_type,
                     unit_size,
                     start_time,
                     end_time}).

-record(building_queue, {id,
                         city_id,
                         building_type,
                         start_time,
                         end_time}).

-record(improvement_queue, {improvement_id,
                            player_id,
                            start_time,
                            end_time}).

%%% Type tables %%%
-record(unit_type, {id,
                    name,
                    level,
                    attack,
                    defense,
                    speed,
                    max_hp,
                    movement,
                    cost}).

-record(population_type, {id,
                          name}).

-record(resource_type, {id,
                        name}).

-record(improvement_type, {id,
                           name}).

-record(building_type, {id,
                        name}).










