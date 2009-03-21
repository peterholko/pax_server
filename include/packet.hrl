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
-record(info_army, {hero,
					units}).

-define (CMD_INFO_CITY, 53).
-record(info_city, {buildings}).

-define (CMD_INFO_UNIT_QUEUE, 55).
-record(info_unit_queue, {building_type,
                          queue_units}).