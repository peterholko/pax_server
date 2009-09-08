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

-record(unit_type, {id,
                    name,
                    level,
                    attack,
                    defense,
                    speed,
					max_hp,
                    movement,
                    cost}).

-record(city, { id,
               	player_id,
               	x,
               	y, 
               	state = 0,
                buildings = [],
                units = []}).

-record(building_type, {id,
                        name,
                        type}).

-record(unit_queue, {id,
                     city_id,
                     unit_type,
                     unit_size,
                     start_time,
                     end_time}).

-record(battle, {id,
                 armies = [],
				 x,
				 y}).







