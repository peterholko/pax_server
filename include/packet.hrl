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

-define(CMD_INFO_KINGDOM, 6).
-record(info_kingdom, {id,
                       name,
                       gold}).

-define(CMD_SUCCESS, 20).
-record(success, {type,
                  id}).

-define(CMD_EXPLORED_MAP, 39).
-record(map, {tiles}).

-define(CMD_PERCEPTION, 40).
-record(perception, {entities,
                     tiles}).

-define(CMD_MOVE, 42).
-record(move, {id,
               x,
               y}).

-define(CMD_ATTACK, 43).
-record(attack, {id,
                 target_id}).

-define(CMD_ADD_WAYPOINT, 44).
-record(add_waypoint, {id,
                       x,
                       y}).

-define (CMD_REQUEST_INFO, 50).
-record(request_info, {type,
                       id}).

-define (CMD_INFO, 51).
-record(info, {info_list}).

-define (CMD_INFO_TILE, 52).
-record(info_tile, {tile_index,
                    tile_type,
                    resources}).

-define (CMD_INFO_ARMY, 53).
-record(info_army, {id,
                    name,
                    kingdom_name,                   
                    units,
                    items}).


-define (CMD_INFO_CITY, 54).
-record(info_city, {id,
                    name,
                    buildings,
                    buildings_queue,
                    units,
                    units_queue,
                    claims,
                    improvements,
                    assignments,
                    items,
                    populations}).

-define (CMD_TRANSPORT_INFO, 55).
-record(transport_info, {transport_id, 
                         units}).

-define (CMD_INFO_GENERIC_ARMY, 56).
-record(info_generic_army, {id,
                            player_id,
                            name,
                            kingdom_name}).

-define (CMD_INFO_GENERIC_CITY, 57).
-record(info_generic_city, {id,
                            player_id,
                            name,
                            kingdom_name}).

-define (CMD_CITY_QUEUE_BUILDING, 59). 
-record(city_queue_building, {building_id,
                              city_id,
                              building_type}).

-define (CMD_CITY_QUEUE_UNIT, 60).
-record(city_queue_unit, {id,
                          unit_type,
                          unit_size,
                          caste,
                          race}).

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

-define (CMD_BATTLE_REMOVE_ARMY, 72).
-record(battle_remove_army, {battle_id, 
                             army}).

-define (CMD_BATTLE_DAMAGE, 74).
-record(battle_damage, {battle_id,
                        source_id,
                        target_id,
                        damage}).

-define (CMD_BATTLE_TARGET, 75).
-record(battle_target, {battle_id,
                        source_army_id,
                        source_unit_id,
                        target_army_id,
                        target_unit_id}).

-define (CMD_BATTLE_RETREAT, 76).
-record(battle_retreat, {battle_id,
                         source_army_id}). 

-define(CMD_BATTLE_LEAVE, 77).
-record(battle_leave, {battle_id,
                       source_army_id}).

-define(CMD_BUILD_IMPROVEMENT, 100).
-record(build_improvement, {city_id,
                            x,
                            y,
                            improvement_type}).

-define(CMD_ADD_CLAIM, 125).
-record(add_claim, {city_id,
                    x,
                    y}).

-define(CMD_ASSIGN_TASK, 130).
-record(assign_task, {city_id,
                      caste,
                      race,
                      amount,
                      task_id,
                      task_type}).


-define(CMD_TRANSFER_ITEM, 150).
-record(transfer_item, {item_id,
                        source_id,
                        source_type,
                        target_id,
                        target_type}).

-define(CMD_DELETE_ITEM, 151).
-record(delete_item, {item_id}).

-define(CMD_CREATE_SELL_ORDER, 170).
-record(create_sell_order, {item_id,
                            price}).

-define(CMD_CREATE_BUY_ORDER, 171).
-record(create_buy_order, {city_id,
                           item_type,
                           volume,
                           price}).

-define(CMD_FILL_SELL_ORDER, 172).
-record(fill_sell_order, {order_id,
                          volume}).


-define(CMD_FILL_BUY_ORDER, 173).
-record(fill_buy_order, {order_id,
                         volume}).

-record(tt, {test}).
