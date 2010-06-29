%% Author: Peter
%% Created: Dec 15, 2008
%% Description: TODO: Add description to db
-module(db).

%%
%% Include files
%%
-import(lists, [reverse/1, foreach/2]).

-include("game.hrl").
-include("schema.hrl").

-include_lib("stdlib/include/qlc.hrl").

%%
%% Exported Functions
%%
-export([create_schema/0, start/0, 
         write/1, read/2, delete/2, index_read/3,
         dirty_write/1, dirty_read/2, dirty_index_read/3, dirty_delete/2, dirty_match_object/1,
         reset_game_tables/0, reset_tables/0, dump/1, select_armies/0,
         select_cities/0, select_battles/0, 
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

    {atomic, ok} = mnesia:create_table(player, [{disc_copies, [node()]}, {attributes, record_info(fields, player)}]),
    {atomic, ok} = mnesia:create_table(connection, [{disc_copies, [node()]}, {attributes, record_info(fields, connection)}]),
    {atomic, ok} = mnesia:create_table(kingdom, [{disc_copies, [node()]}, {attributes, record_info(fields, kingdom)}]),
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
 
    {atomic, ok} = mnesia:create_table(unit_type, [{disc_copies, [node()]}, {attributes, record_info(fields, unit_type)}]),
    {atomic, ok} = mnesia:create_table(building_type, [{disc_copies, [node()]}, {attributes, record_info(fields, building_type)}]),
    {atomic, ok} = mnesia:create_table(resource_type, [{disc_copies, [node()]}, {attributes, record_info(fields, resource_type)}]),
    {atomic, ok} = mnesia:create_table(population_type, [{disc_copies, [node()]}, {attributes, record_info(fields, population_type)}]),
    {atomic, ok} = mnesia:create_table(improvement_type, [{disc_copies, [node()]}, {attributes, record_info(fields, improvement_type)}]),
 
    {atomic, ok} = mnesia:create_table(item_type_ref, [{disc_copies, [node()]}, {attributes, record_info(fields, item_type_ref)}]),    
    {atomic, ok} = mnesia:create_table(player_type, [{disc_copies, [node()]}, {attributes, record_info(fields, player_type)}]),    

    {atomic, ok} = mnesia:create_table(unit_queue, [{disc_copies, [node()]}, {attributes, record_info(fields, unit_queue)}]),
    {atomic, ok} = mnesia:create_table(building_queue, [{disc_copies, [node()]}, {attributes, record_info(fields, building_queue)}]),

    mnesia:add_table_index(player, name),
    mnesia:add_table_index(kingdom, player_id),
    mnesia:add_table_index(unit, entity_id),
    mnesia:add_table_index(building, city_id),
    mnesia:add_table_index(claim, tile_index),
    mnesia:add_table_index(claim, city_id),
    mnesia:add_table_index(assignment, city_id),
    mnesia:add_table_index(assignment, population_id),
    mnesia:add_table_index(improvement, tile_index),
    mnesia:add_table_index(improvement, city_id),
    mnesia:add_table_index(population, city_id),

    mnesia:add_table_index(unit_queue, city_id),
    mnesia:add_table_index(building_queue, city_id),

    mnesia:stop().

start() ->
    mnesia:start(),
    mnesia:wait_for_tables([connection, army, unit, unit_type, hero, city, battle, building_type, unit_queue, counter, player, claim], 5000).

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
    [{unit_type, 1, "Footsolider", 	1, 4, 2, 5, 1, 5, 10},
     {unit_type, 2, "Archer", 		2, 5, 1, 10, 2, 5, 10},
     {building_type, 1, "Barracks", 20},
     {building_type, 2, "Weaponsmith", 4}].

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
     {kingdom, 1, 1, 5000, [1,2], [11]},
     {kingdom, 2, 2, 100, [3], [12]},
     {kingdom, 3, 3, 100, [4], [13]},
     {kingdom, 4, 4, 100, [5], []},
     {player_type, 1, 0},
     {player_type, 2, 0},
     {player_type, 3, 0},
     {player_type, 4, 0},
     {player_type, -1, 1},
     {city, 11, 1, 3, 4, 0, {0, nil}, {2,{5,nil,{6,nil,nil}}}, [], 0},
     %{city, 12, 2, 1, 5, 0, {0, nil}, {0, nil}, [], 0},
     %{city, 13, 3, 7, 4, 0, {0, nil}, {0, nil}, [], 0},
     {army, 1, 1,  2,  2, 0, 0, none, 0, 1, {2,{1,nil,{2,nil,nil}}}, none},
     {army, 2, 1,  5,  5, 0, 0, none, 0, 0, {2,{7,nil,{8,nil,nil}}}, none},
     {army, 3, 2,  3,  3, 0, 0, none, 0, 0, {1,{9,nil,nil}}, none},
     {army, 4, 3, 10, 10, 0, 0, none, 0, 0, {0, nil}, none},
     {army, 5, 4, 15,  2, 0, 0, none, 0, 0, {0, nil}, none},
     {army, 6, -1, 3, 1, 0, 0, none, 0, 0, {1,{3,nil, nil}}, none},
     {hero, 1, 1, 1},
     {unit, 1, 1, 1, 1, 25, 1},
     {unit, 2, 1, 1, 2, 20, 1},
     {unit, 3, 6, 1, 1, 16, 1},
     {unit, 4, 4, 1, 1, 25, 1},
     {unit, 5, 11, 2, 1, 20, 1},
     {unit, 6, 11, 2, 2, 500, 1},
     {unit, 7, 2, 1, 2, 20, 1},
     {unit, 8, 2, 1, 1, 30, 1},
     {unit, 9, 3, 1, 1, 15, 1},
     {population, {11, 0}, 11, 0, 100},
     {population, {11, 1}, 11, 1, 0},
     {population, {11, 2}, 11, 2, 0},
     {population, {11, 3}, 11, 3, 0},
     {transport, 1, 1, gb_sets:new()}    
    ].

reset_tables() ->
    F = fun() ->
                foreach(fun mnesia:write/1, test_tables())
        end,    
    mnesia:transaction(F).    
