-define(ERR_UNKNOWN, 0).
-define(ERR_BAD_LOGIN, 1).
-define(ERR_ACCOUNT_DISABLED, 2).

-define(MAP_NUMTILES, 2500).
-define(MAP_WIDTH, 50).
-define(MAP_HEIGHT, 50).
-define(MAP_BLOCK_WIDTH, 7).
-define(MAP_BLOCK_HEIGHT, 7).
-define(MAP_FILENAME, "map.bin").

-define(STATE_NONE, 0).
-define(STATE_MOVE, 1).
-define(STATE_ATTACK, 2).
-define(STATE_COMBAT, 3).

-define(EVENT_NONE, 0).
-define(EVENT_MOVE, 1).
-define(EVENT_ATTACK, 2).

-define(GAME_LOOP, 200).

-define(OBJECT_TILE, 0).
-define(OBJECT_ARMY, 1).
-define(OBJECT_CITY, 2).
-define(OBJECT_BUILDING, 3).

-define(BUILDING_UNIT_LAND, 1).
-define(BUILDING_UNIT_SEA, 2).
-define(BUILDING_UNIT_AIR, 3).