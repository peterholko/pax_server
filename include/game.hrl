-record(player_process, {
                     player_id,
                     process                 
                     }).

-record(game_info, {
                    players = [],
                    characters = [],
                    npcs = [],
                    objects = []
                    }).