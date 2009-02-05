%% Author: Peter
%% Created: Dec 15, 2008
%% Commands in Packets
-include("common.hrl").

-define(CMD_POLICYREQUEST, <<60,112,111,108,105,99,121,45,102,105,108,101,45,114,101,113,117,101,115,116,47,62,0>>).
-define(CMD_POLICY, <<"<cross-domain-policy>\n<allow-access-from domain=\"*\" to-ports=\"*\" />\n</cross-domain-policy>", 0>>).

-define(CMD_BAD, 255).

-record(bad, {
					cmd, 
					error = ?ERR_UNKNOWN
				 }).



-define(CMD_LOGIN, 1).

-record(login, {
					name,
					pass
				 }).

-define(CMD_LOGOUT, 2).

-record(logout, {
				 }).

-define(CMD_CLOCKSYNC, 3).
-define(CMD_CLIENTREADY, 4).

-define(CMD_PLAYER_ID, 5).

-record(player_id, {
					id
				 }).

-define(CMD_MAP, 39).

-record(map, {
              coords, %coordinates of the top left corner of the block of tiles
              tiles
             }).

-define(CMD_PERCEPTION, 40).

-record(perception, {
					characters
				 }).

-define (CMD_MOVE, 42).

-record(move, { 
               direction
                }).