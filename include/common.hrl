-define(ERR_UNKNOWN, 0).
-define(ERR_BAD_LOGIN, 1).
-define(ERR_ACCOUNT_DISABLED, 2).

-define(MAP_NUMTILES, 2500).
-define(MAP_WIDTH, 50).
-define(MAP_HEIGHT, 50).
-define(MAP_BLOCK_WIDTH, 7).
-define(MAP_BLOCK_HEIGHT, 7).
-define(MAP_FILENAME, "map.bin").

-define(STATE_NO_VISION, -1).
-define(STATE_NONE, 0).
-define(STATE_MOVE, 1).
-define(STATE_ATTACK, 2).
-define(STATE_COMBAT, 3).
-define(STATE_DEAD, 4).
-define(STATE_CONSTRUCTING, 5).
-define(STATE_COMPLETED, 6).

-define(EVENT_NONE, 0).
-define(EVENT_MOVE, 1).
-define(EVENT_ATTACK, 2).
-define(EVENT_UNIT_ROUND, 3).
-define(EVENT_IMPROVEMENT_COMPLETED, 4).
-define(EVENT_HARVEST, 5).
-define(EVENT_GROWTH, 6).

-define(GAME_LOOP_TICK, 200).

-define(OBJECT_TILE, 0).
-define(OBJECT_ARMY, 1).
-define(OBJECT_CITY, 2).
-define(OBJECT_BUILDING, 3).
-define(OBJECT_BATTLE, 4).
-define(OBJECT_IMPROVEMENT, 5).
-define(OBJECT_TRANSPORT, 6).

-define(BUILDING_UNIT_LAND, 1).
-define(BUILDING_UNIT_SEA, 2).
-define(BUILDING_UNIT_AIR, 3).

-define(IMPROVEMENT_FARM, 0).

-define(ITEM_FOOD, 0).

-define(ARMY_DEAD, 0).
-define(ARMY_ALIVE, 1).

-define(PLAYER_NONE, -1).
-define(PLAYER_HUMAN, 0).
-define(PLAYER_COMPUTER, 1).
-define(PLAYER_GM, 2).

-define(HARVEST_TICK, 100).
-define(GROWTH_TICK, 500).

-define(CASTE_SLAVE, 0).
-define(CASTE_SOLDIER, 1).
-define(CASTE_COMMONER, 2).
-define(CASTE_NOBLE, 3).
