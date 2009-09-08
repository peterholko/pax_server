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
                    events = []
                    }).

-record(state, { id,
				 player_id,
                 type,
                 state,
                 x,              
                 y}).
