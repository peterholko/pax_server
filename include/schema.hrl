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
            	units = []}).

-record(hero, {id,
               army_id,
               level}).

-record(unit, {id,
               army_id,
               type_id,
               size,
               hp}).

-record(unit_type, {id,
                    name,
                    level,
                    attack,
                    defense,
					max_hp,
                    speed,
                    cost}).

-record(city, { id,
               	player_id,
               	x,
               	y, 
               	state = 0,
                buildings = []}).

-record(building_type, {id,
                        name,
                        type}).

-record(unit_queue, {player_id,
                     building_type,
                     unit_type,
                     unit_amount,
                     start_time,
                     build_time}).






