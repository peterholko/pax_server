%% Author: Peter
%% Created: Dec 15, 2008
%% Commands in Packets
-include("common.hrl").

-define(CMD_POLICYREQUEST, <<60,112,111,108,105,99,121,45,102,105,108,101,45,114,101,113,117,101,115,116,47,62,0>>).
-define(CMD_POLICY, <<"<cross-domain-policy>\n<allow-access-from domain=\"*\" to-ports=\"*\" />\n</cross-domain-policy>", 0>>).

-define(CMD_BAD, 255).
-record(bad, {cmd,
              error = ?ERR_UNKNOWN}).

-define(CMD_LOGIN, 1).
-record(login, {name,
                pass}).

-define(CMD_LOGOUT, 2).
-record(logout, {}).

-define(CMD_CLOCKSYNC, 3).
-define(CMD_CLIENTREADY, 4).

-define(CMD_PLAYER_ID, 5).
-record(player_id, {id}).

-define(CMD_EXPLORED_MAP, 39).
-record(map, {tiles}).

-define(CMD_PERCEPTION, 40).
-record(perception, {entities,
                     tiles}).

-define (CMD_MOVE, 42).
-record(move, {id,
               x,
               y}).

-define (CMD_ATTACK, 43).
-record(attack, {id,
                 target_id}).

-define (CMD_REQUEST_INFO, 50).
-record(request_info, {type,
                       id}).

-define (CMD_INFO, 51).
-record(info, {info_list}).

-define (CMD_INFO_ARMY, 52).
-record(info_army, {id,
                    units}).

-define (CMD_INFO_CITY, 53).
-record(info_city, {id,
                    buildings,
                    buildings_queue,
                    units,
                    units_queue}).

-define (CMD_TRANSPORT_INFO, 55).
-record(transport_info, {transport_id, 
                         units}).

-define (CMD_CITY_QUEUE_BUILDING, 59). 
-record(city_queue_building, {id,
                              building_type}).

-define (CMD_CITY_QUEUE_UNIT, 60).
-record(city_queue_unit, {id,
                          unit_type,
                          unit_size}).

-define (CMD_TRANSFER_UNIT, 61).
-record(transfer_unit, {unit_id,
                        source_id,
                        source_type,
                        target_id,
                        target_type}).

-define (CMD_BATTLE_INFO, 70).
-record(battle_info, {battle_id,
                      armies}).

-define (CMD_BATTLE_ADD_ARMY, 71).
-record(battle_add_army, {battle_id,
                          army}).

-define (CMD_BATTLE_DAMAGE, 72).
-record(battle_damage, {battle_id,
                        source_id,
                        target_id,
                        damage}).

-define (CMD_BATTLE_TARGET, 73).
-record(battle_target, {battle_id,
                        source_army_id,
                        source_unit_id,
                        target_army_id,
                        target_unit_id}).

-define(CMD_BUILD_IMPROVEMENT, 100).
-record(build_improvement, {city_id,
                            x,
                            y,
                            improvement_type}).

-define(CMD_ADD_CLAIM, 125).
-record(add_claim, {city_id,
                    x,
                    y}).

-record(tt, {test}).
