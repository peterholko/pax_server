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
                    objects = [],
                    events = []
                    }).

-record(entity, {
                 id,
                 x,
                 y,
                 player_id              
                 }).