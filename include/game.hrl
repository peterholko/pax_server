-record(player_process, {
                     player_id,
                     process                 
                     }).

-record(game_info, {
                    players = [],
                    entities = [],
                    armies = [],
                    cities = [],
                    npcs = [],
                    objects = []
                    }).

-record(entity, {
                 id,
                 x,
                 y,
                 player_id              
                 }).