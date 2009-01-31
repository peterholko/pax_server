-record(player, {
					id,
					name,
					password,
					login_errors = 0,
					disabled = false,
                    character_id
				 }).

-record(connection, {
					player_id,
					process = none,
					socket = none
				 }).

-record(character, {
                     player_id,
                     x,
                     y,
                     z,
                     action = none  
                     }).

