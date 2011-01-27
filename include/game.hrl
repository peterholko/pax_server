-record(player_process, {
                     player_id,
                     process                 
                     }).

-record(game_info, {
                    tick = 0,
                    players = [],
                    entities = [],
                    armies = [],
                    cities = [],
                    npcs = [],
                    battles = [],
                    objects = [],
                    events = [],
					update_perceptions
                    }).

-record(state, { id,
				 player_id,
                 type,
                 subtype,
                 state,
                 x,              
                 y}).
