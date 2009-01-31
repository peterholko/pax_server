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

-define (CMD_MOVE, 3).

-record(move, { 
               direction
                }).

-define (CMD_CLOCKSYNC, 4).

-define (CMD_PING, 5).

-define(CMD_PLAYER_ID, 31).

-record(player_id, {
					id
				 }).

-define(CMD_PERCEPTION, 40).

-record(perception, {
					characters
				 }).

-define(CMD_MAP, 41).

-record(map, {
              tiles
             }).