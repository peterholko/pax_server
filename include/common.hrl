-define(MAX_TIME, 2147483647).
-define(MAX_INT, 2147483647).

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
-define(STATE_RETREAT, 7).
-define(STATE_RETREAT_MOVE, 8).
-define(STATE_LEAVE, 9).
-define(STATE_LEAVE_MOVE, 10).
-define(STATE_IN_PROGRESS, 11).
-define(STATE_CLAIM, 12).
-define(STATE_EMPTY, 13).

-define(EVENT_NONE, 0).
-define(EVENT_MOVE, 1).
-define(EVENT_ATTACK, 2).
-define(EVENT_UNIT_ATTACK, 3).
-define(EVENT_RETREAT, 7).
-define(EVENT_RETREAT_MOVE, 8).
-define(EVENT_LEAVE, 9).
-define(EVENT_LEAVE_MOVE, 10).
-define(EVENT_GROWTH, 20).
-define(EVENT_CLAIM, 21).
-define(EVENT_IMPROVEMENT_COMPLETED, 30).

-define(TRIGGER_MOVE, 1).

-define(GAME_LOOP_TICK, 200).
-define(GAME_VISION_RANGE, 50).
-define(GAME_NUM_HOURS_PER_DAY, 6).

-define(OBJECT_NONE, -1).
-define(OBJECT_TILE, 0).
-define(OBJECT_ARMY, 1).
-define(OBJECT_CITY, 2).
-define(OBJECT_BUILDING, 3).
-define(OBJECT_BATTLE, 4).
-define(OBJECT_IMPROVEMENT, 5).
-define(OBJECT_UNIT, 6).
-define(OBJECT_ITEM_RECIPE, 7).
-define(OBJECT_BASIC, 255).

-define(BUILDING_UNIT_LAND, 1).
-define(BUILDING_UNIT_SEA, 2).
-define(BUILDING_UNIT_AIR, 3).

-define(IMPROVEMENT_FARM, 0).

-define(ITEM_FOOD, 1036).

-define(ARMY_DEAD, 0).
-define(ARMY_ALIVE, 1).

-define(PLAYER_NONE, -1).
-define(PLAYER_HUMAN, 0).
-define(PLAYER_COMPUTER, 1).
-define(PLAYER_GM, 2).

-define(CLAIM_TICK, 300).
-define(GROWTH_TICK, 300).

-define(CASTE_SLAVE, 0).
-define(CASTE_SOLDIER, 1).
-define(CASTE_COMMONER, 2).
-define(CASTE_NOBLE, 3).

-define(RACE_HUMAN, 0).
-define(RACE_ELF, 1).
-define(RACE_DWARF, 2).
-define(RACE_GOBLIN, 3).

-define(TASK_BUILDING, 0).
-define(TASK_UNIT, 1).
-define(TASK_IMPROVEMENT, 2).
-define(TASK_ITEM, 3).
-define(TASK_HARVEST, 4).

-define(TILE_MOUNTAIN, 0).
-define(TILE_FOREST, 1).
-define(TILE_PLAINS, 2).
-define(TILE_SWAMP, 3).

-define(TILE_MOUNTAIN_SPEED, 10 / 25).
-define(TILE_FOREST_SPEED, 4 / 25).
-define(TILE_PLAINS_SPEED, 1 / 25).
-define(TILE_SWAMP_SPEED, 8 / 25).

-define(TILE_MOUNTAIN_AMBUSH, 0.50).
-define(TILE_FOREST_AMBUSH, 0.25).
-define(TILE_PLAINS_AMBUSH, 0.10).
-define(TILE_SWAMP_AMBUSH, 0.33).

-define(BASE_SPEED, 25).

-define(MARKET_BUY, 0).
-define(MARKET_SELL, 1).

-define(CASTE_SLAVE_RATE, 50).
-define(CASTE_SOLDIER_RATE, 150).
-define(CASTE_COMMONER_RATE, 200).
-define(CASTE_NOBLE_RATE, 100).

-define(CASTE_COMMONER_REVENUE, 1).
-define(CASTE_NOBLE_REVENUE, 10).

-define(CONTRACT_BUILDING, 0).
-define(CONTRACT_UNIT, 1).
-define(CONTRACT_IMPROVEMENT, 2).
-define(CONTRACT_ITEM, 3).
-define(CONTRACT_HARVEST, 4).

-define(BATTLE_ADD_ARMY, 0).
-define(BATTLE_REMOVE_ARMY, 1).
-define(BATTLE_RETREAT, 2).
-define(BATTLE_MOVE, 3).

-define(ITEM_REQ_NONE, -1).
-define(ITEM_RECIPE_ID_OFFSET, 10000).
-define(ITEM_STONE1, 1041).
-define(ITEM_STONE2, 1042).
-define(ITEM_LUMBER1, 1034).

-define(TAX_COMMONER, 1).
-define(TAX_NOBLE, 2).
-define(TAX_TARIFF, 3).

-define(INFO(MSG), log4erl:info("{~w} ~s", [?MODULE, MSG])).
-define(INFO(MSG, DATA), log4erl:info("{~w} ~s ~w", [?MODULE, MSG, DATA])).
-define(INFO2(MSG, DATA), io:fwrite("~s ~s~n", [MSG, DATA])).
-define(INFO(MSG1, DATA1, MSG2, DATA2), log4erl:info("{~w} ~s ~w ~s ~w", [?MODULE, MSG1, DATA1, MSG2, DATA2])).
-define(ERROR(MSG), log4erl:error("{~w:~w} ~s", [?MODULE, ?LINE, MSG])).
-define(ERROR(MSG, DATA), log4erl:error("{~w:~w} ~s: ~w", [?MODULE, ?LINE, MSG, DATA])).
