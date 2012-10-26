%% Author: Peter
%% Created: Dec 15, 2008
%% Description: TODO: Add description to db
-module(db).

%%
%% Include files
%%
-import(lists, [reverse/1, foreach/2]).

-include("common.hrl").
-include("game.hrl").
-include("schema.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("stdlib/include/qlc.hrl").

%%
%% Exported Functions
%%
-export([create_schema/0, start/0, 
         write/1, read/2, delete/2, index_read/3,
         dirty_write/1, dirty_read/2, dirty_index_read/3, dirty_delete/2, dirty_match_object/1,
         dirty_delete_object/1,
         reset_game_tables/0, reset_tables/0, dump/1, 
         select_armies/0, select_cities/0, select_battles/0, 
         select_improvements/1, 
         select_all_armies/0, select_all_players/0,
         do/1
        ]).

%%
%% API Functions
%%

create_schema() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),

    ok = application:set_env( mnesia, dump_log_write_threshold, 10000 ), 

    {atomic, ok} = mnesia:create_table(player, [{disc_copies, [node()]}, {attributes, record_info(fields, player)}]),
    {atomic, ok} = mnesia:create_table(connection, [{disc_copies, [node()]}, {attributes, record_info(fields, connection)}]),
    {atomic, ok} = mnesia:create_table(kingdom, [{disc_copies, [node()]}, {attributes, record_info(fields, kingdom)}]),
    {atomic, ok} = mnesia:create_table(entity, [{disc_copies, [node()]}, {attributes, record_info(fields, entity)}]),
    {atomic, ok} = mnesia:create_table(army, [{disc_copies, [node()]}, {attributes, record_info(fields, army)}]),
    {atomic, ok} = mnesia:create_table(unit, [{disc_copies, [node()]}, {attributes, record_info(fields, unit)}]),    
    {atomic, ok} = mnesia:create_table(hero, [{disc_copies, [node()]}, {attributes, record_info(fields, hero)}]),
    {atomic, ok} = mnesia:create_table(city, [{disc_copies, [node()]}, {attributes, record_info(fields, city)}]),
    {atomic, ok} = mnesia:create_table(battle, [{disc_copies, [node()]}, {attributes, record_info(fields, battle)}]),
    {atomic, ok} = mnesia:create_table(counter, [{disc_copies, [node()]}, {attributes, record_info(fields, counter)}]),
    {atomic, ok} = mnesia:create_table(improvement, [{disc_copies, [node()]}, {attributes, record_info(fields, improvement)}]),
    {atomic, ok} = mnesia:create_table(transport, [{disc_copies, [node()]}, {attributes, record_info(fields, transport)}]),
    {atomic, ok} = mnesia:create_table(building, [{disc_copies, [node()]}, {attributes, record_info(fields, building)}]),
    {atomic, ok} = mnesia:create_table(population, [{disc_copies, [node()]}, {attributes, record_info(fields, population)}]),
    {atomic, ok} = mnesia:create_table(claim, [{disc_copies, [node()]}, {attributes, record_info(fields, claim)}]),   
    {atomic, ok} = mnesia:create_table(item, [{disc_copies, [node()]}, {attributes, record_info(fields, item)}]),   
    {atomic, ok} = mnesia:create_table(assignment, [{disc_copies, [node()]}, {attributes, record_info(fields, assignment)}]),    
    {atomic, ok} = mnesia:create_table(market_item, [{disc_copies, [node()]}, {attributes, record_info(fields, market_item)}]),   

    {atomic, ok} = mnesia:create_table(trade_route, [{disc_copies, [node()]}, {attributes, record_info(fields, trade_route)}]),  
    {atomic, ok} = mnesia:create_table(market_order, [{disc_copies, [node()]}, {attributes, record_info(fields, market_order)}]),   
    {atomic, ok} = mnesia:create_table(reputation, [{disc_copies, [node()]}, {attributes, record_info(fields, reputation)}]),   

    {atomic, ok} = mnesia:create_table(tile, [{ram_copies, [node()]}, {attributes, record_info(fields, tile)}]),   
    {atomic, ok} = mnesia:create_table(resource, [{ram_copies, [node()]}, {attributes, record_info(fields, resource)}]),   

    {atomic, ok} = mnesia:create_table(map_object, [{ram_copies, [node()]}, {attributes, record_info(fields, map_object)}]),   

    {atomic, ok} = mnesia:create_table(building_type, [{disc_copies, [node()]}, {attributes, record_info(fields, building_type)}]),
    {atomic, ok} = mnesia:create_table(resource_type, [{disc_copies, [node()]}, {attributes, record_info(fields, resource_type)}]),
    {atomic, ok} = mnesia:create_table(population_type, [{disc_copies, [node()]}, {attributes, record_info(fields, population_type)}]),
    {atomic, ok} = mnesia:create_table(improvement_type, [{disc_copies, [node()]}, {attributes, record_info(fields, improvement_type)}]), 
    {atomic, ok} = mnesia:create_table(item_base, [{disc_copies, [node()]}, {attributes, record_info(fields, item_base)}]),
    {atomic, ok} = mnesia:create_table(item_category, [{disc_copies, [node()]}, {attributes, record_info(fields, item_category)}]),
    {atomic, ok} = mnesia:create_table(item_template, [{disc_copies, [node()]}, {attributes, record_info(fields, item_template)}]),
    {atomic, ok} = mnesia:create_table(item_recipe, [{disc_copies, [node()]}, {attributes, record_info(fields, item_recipe)}]),
    {atomic, ok} = mnesia:create_table(unit_template, [{disc_copies, [node()]}, {attributes, record_info(fields, unit_template)}]),
    {atomic, ok} = mnesia:create_table(unit_recipe, [{disc_copies, [node()]}, {attributes, record_info(fields, unit_recipe)}]),

    {atomic, ok} = mnesia:create_table(item_type_ref, [{disc_copies,  [node()]}, {type, bag}, {attributes, record_info(fields, item_type_ref)}]),    
    {atomic, ok} = mnesia:create_table(player_type, [{disc_copies, [node()]}, {attributes, record_info(fields, player_type)}]),    
    {atomic, ok} = mnesia:create_table(reputation_ref, [{disc_copies, [node()]}, {attributes, record_info(fields, reputation_ref)}]),    

    {atomic, ok} = mnesia:create_table(contract, [{disc_copies, [node()]}, {attributes, record_info(fields, contract)}]),
    {atomic, ok} = mnesia:create_table(unit_queue, [{disc_copies, [node()]}, {attributes, record_info(fields, unit_queue)}]),
    {atomic, ok} = mnesia:create_table(building_queue, [{disc_copies, [node()]}, {attributes, record_info(fields, building_queue)}]),
    {atomic, ok} = mnesia:create_table(improvement_queue, [{disc_copies, [node()]}, {attributes, record_info(fields, improvement_queue)}]),
    {atomic, ok} = mnesia:create_table(item_queue, [{disc_copies, [node()]}, {attributes, record_info(fields, item_queue)}]),

    mnesia:add_table_index(player, name),
    mnesia:add_table_index(kingdom, player_id),
    mnesia:add_table_index(unit, entity_id),
    mnesia:add_table_index(claim, tile_index),
    mnesia:add_table_index(claim, city_id),
    mnesia:add_table_index(claim, army_id),
    mnesia:add_table_index(improvement, tile_index),
    mnesia:add_table_index(improvement, city_id),
    mnesia:add_table_index(population, city_id),   

    mnesia:add_table_index(item, ref),
    mnesia:add_table_index(item_type_ref, item_id),
    mnesia:add_table_index(item_recipe, player_id),
    mnesia:add_table_index(item_category, display_name),

    mnesia:add_table_index(unit_recipe, player_id),

    mnesia:add_table_index(building, city_id),
    mnesia:add_table_index(building_type, name),

    mnesia:add_table_index(market_order, city_id),
    mnesia:add_table_index(market_order, player_id),

    mnesia:add_table_index(contract, city_id),
    mnesia:add_table_index(contract, target_ref),
    
    mnesia:add_table_index(assignment, city_id),
    mnesia:add_table_index(assignment, target_ref),

    mnesia:stop().

start() ->
    mnesia:start(),
    mnesia:wait_for_tables([connection, army, unit,  hero, city, battle, building_type, unit_queue, counter, player, claim], 5000).

write(R) ->
    F = fun() -> mnesia:write(R) end,
    {atomic, Value} = mnesia:transaction(F),
    Value.  

read(T, K) ->
    F = fun() -> mnesia:read(T, K) end,
    {atomic, Value} = mnesia:transaction(F),
    Value.

delete(T, K) ->
    F = fun() -> mnesia:delete(T, K, write) end,
    {atomic, Value} = mnesia:transaction(F),
    Value.  

index_read(T, V, K) ->
    F = fun() ->  mnesia:index_read(T, V, K) end,
    {atomic, Value} = mnesia:transaction(F),
    Value.

dirty_read(T, K) ->
    mnesia:dirty_read(T, K).

dirty_index_read(T, V, K) ->
    mnesia:dirty_index_read(T, V, K).

dirty_write(R) ->
    mnesia:dirty_write(R).

dirty_delete(T, K) ->
    mnesia:dirty_delete(T, K). 

dirty_delete_object(P) ->
    mnesia:dirty_delete_object(P).

dirty_match_object(P) ->
    mnesia:dirty_match_object(P).

dump(Table) ->
    do(qlc:q([X || X <- mnesia:table(Table)])).

select_armies() ->
    do(qlc:q([{X#army.id, X#army.player_id} || X <- mnesia:table(army)])).

select_cities() ->
    do(qlc:q([{X#city.id, X#city.player_id} || X <- mnesia:table(city)])).

select_battles() ->
    do(qlc:q([{X#battle.id} || X <- mnesia:table(battle)])).

select_improvements(ImprovementPid) ->
    do(qlc:q([{X#improvement.id, ImprovementPid} || X <- mnesia:table(improvement)])).

select_all_armies() ->
    do(qlc:q([X || X <- mnesia:table(army)])).

select_all_players() ->
    do(qlc:q([X || X <- mnesia:table(player)])).

%%
%% Local Functions
%%

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

%% Table Data
game_tables() ->
    [{unit_type, 1, "Footsolider", 	1, 4, 2, 5, 1, 5, 10, 10},
     {unit_type, 2, "Archer", 		2, 5, 1, 10, 2, 5, 10, 10}].
    % {building_type, 42, "Barracks", 100, 100, 1},
    % {building_type, 267, "Market", 100, 200, 1},
    % {building_type, 3, "Temple", 100, 150, 1}].
     %{improvement_type, 1, "Farm", 500, 100},
     %{improvement_type, 6, "Trapper", 100, 100},
     %{improvement_type, 3, "Lumbermill", 100, 100},
     %{improvement_type, 4, "Mine", 100, 100},
     %{improvement_type, 5, "Quarry", 100, 100}].
reset_game_tables() ->
    F = fun() ->
                foreach(fun mnesia:write/1, game_tables())
        end,    
    mnesia:transaction(F).    

%% Testing

test_tables() ->
    [
     {connection, 1, none, none},
     {connection, 2, none, none},
     {connection, 3, none, none},
     {connection, 4, none, none},     
     {player, 1, <<"test1">>, <<"123123">>, 0, false},
     {player, 2, <<"test2">>, <<"123123">>, 0, false},
     {player, 3, <<"test3">>, <<"123123">>, 0, false},
     {player, 4, <<"test4">>, <<"123123">>, 0, false},
     {kingdom, 1, 1, <<"The Rich">>, 5000, [1,2], [11]},
     {kingdom, 2, 2, <<"The Bad">>, 100, [3], []},
     {kingdom, 3, 3, <<"The Ugly">>, 100, [4], []},
     {kingdom, 4, 4, <<"The Poor">>, 100, [5], []},
     {kingdom, -1, -1, <<"The NPC">>, 1000, [6], []},
     {player_type, 1, 0},
     {player_type, 2, 0},
     {player_type, 3, 0},
     {player_type, 4, 0},
     {player_type, -1, 1},
     {entity, 11, 1, ?OBJECT_CITY},
     {entity, 1, 1, ?OBJECT_ARMY},
     {entity, 2, 1, ?OBJECT_ARMY},
     {entity, 3, 2, ?OBJECT_ARMY},
     {entity, 4, 3, ?OBJECT_ARMY},
     {entity, 5, 4, ?OBJECT_ARMY},
     {entity, 6, -1, ?OBJECT_ARMY},
     {city, 11, 1,<<"Calgary">>, 3, 4, 0, {0, nil}, {2,{5,nil,{6,nil,nil}}}, [], 0},
     {army, 1, 1, <<"Army One">>, 3,  7, [], none, none, 0, 1, {2,{1,nil,{2,nil,nil}}}, none},
     {army, 2, 1, <<"Army Two">>, 3,  4, [], none, none, 0, 0, {2,{7,nil,{8,nil,nil}}}, none},
     {army, 3, 2, <<"Army Three">>,  4,  7, [], none, none, 0, 0, {1,{9,nil,nil}}, none},
     {army, 4, 3, <<"Army Four">>, 10, 10, [], none, none, 0, 0, {0, nil}, none},
     {army, 5, 4, <<"Army Five">>, 15,  2, [], none, none, 0, 0, {0, nil}, none},
     {army, 6, -1, <<"NPC Army">>, 7,  7, [], none, none, 0, 0, {2, {3, nil, {10, nil, nil}}}, none},
     {hero, 1, 1, 1},
     {unit, 1, 1, 1, 11, 1000, 1, <<"Test Unit">>, []},
     {unit, 2, 1, 1, 11, 1000, 1, <<"Test Unit">>, []},
     {unit, 3, 6, 1, 11, 16, 1, <<"Test Unit">>, []},
     {unit, 4, 4, 1, 11, 25, 1, <<"Test Unit">>, []},
     {unit, 5, 11, 2, 11, 20, 1, <<"Test Unit">>, []},
     {unit, 6, 11, 2, 11, 500, 1, <<"Test Unit">>, []},
     {unit, 7, 5, 1, 11, 20, 1, <<"Test Unit">>, []},
     {unit, 8, 5, 1, 11, 30, 1, <<"Test Unit">>, []},
     {unit, 9, 3, 1, 11, 1500, 1, <<"Test Unit">>, []},
     {unit, 10, 6, 1, 11, 30, 1, <<"Test Unit">>, []},
     {population, {11, 0, 0}, 11, 0, 0, 10000},
     {population, {11, 1, 1}, 11, 1, 1, 1000},
     {population, {11, 2, 2}, 11, 2, 2, 20000},
     {population, {11, 3, 3}, 11, 3, 3, 3000},
     {population, {11, 0, 1}, 11, 0, 1, 100},
     {population, {11, 0, 2}, 11, 0, 2, 200},
     {claim, 1, 255, 11, 1, 6, 0},
     {item, 9999, {{?OBJECT_CITY, 11}, 1}, 1034, -1, 1000},
     {item, 9998, {{?OBJECT_CITY, 11}, 1}, 1041, -1, 1000},
     {item, 6666, {{?OBJECT_UNIT, 1}, 1}, 1034, -1, 1000},
     {item, 6667, {{?OBJECT_UNIT, 9}, 2}, 1034, -1, 1234},
     {item_type_ref, {{?OBJECT_CITY, 11}, 1, 1034}, 9999},
     {item_type_ref, {{?OBJECT_CITY, 11}, 1, 1041}, 9998},
     {item_type_ref, {{?OBJECT_UNIT, 1}, 1, 1034}, 6666}
     %{improvement, 1, 255, 1, 11, 1, 6, 100, []},
    ].

reset_tables() ->
    counter:reset(entity),

    F = fun() ->
                foreach(fun mnesia:write/1, test_tables())
        end,    
    mnesia:transaction(F).    
