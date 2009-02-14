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
				state = none,			
         	    hero = none,
            	units = none}).

-record(city, { id,
               	player_id,
               	x,
               	y, 
               	state = none}).

-record(explored_map, {
                       player_id,
                       block_x,
                       block_y       
                       }).





