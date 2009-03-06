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
-export([create_schema/0, start/0, write/1, read/2, delete/2, index_read/3,
         reset_game_tables/0, reset_tables/0, select_armies/0,
		 select_cities/0, select_all_armies/0, select_all_players/0,
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
    {atomic, ok} = mnesia:create_table(player, [{attributes, record_info(fields, player)}]),
    {atomic, ok} = mnesia:create_table(connection, [{attributes, record_info(fields, connection)}]),
    {atomic, ok} = mnesia:create_table(army, [{attributes, record_info(fields, army)}]),
    {atomic, ok} = mnesia:create_table(unit, [{attributes, record_info(fields, unit)}]),
    {atomic, ok} = mnesia:create_table(unit_type, [{attributes, record_info(fields, unit_type)}]),
    {atomic, ok} = mnesia:create_table(hero, [{attributes, record_info(fields, hero)}]),
	{atomic, ok} = mnesia:create_table(city, [{attributes, record_info(fields, city)}]),
    {atomic, ok} = mnesia:create_table(counter, [{attributes, record_info(fields, counter)}]),
    
    mnesia:add_table_index(player, name),
    mnesia:stop().

start() ->
    mnesia:start(),
    mnesia:wait_for_tables([player], 5000).

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
    io:fwrite("db:index_read - T: ~w~n", [T]),
  	io:fwrite("db:index_read - V: ~w~n", [V]),
    io:fwrite("db:index_read - K: ~w~n", [K]),
    F = fun() ->  mnesia:index_read(T, V, K) end,
    {atomic, Value} = mnesia:transaction(F),
    io:fwrite("db:index_read - Value: ~w~n", [Value]),
	Value.

select_armies() ->
    do(qlc:q([{X#army.id, X#army.player_id} || X <- mnesia:table(army)])).

select_cities() ->
	do(qlc:q([{X#city.id, X#city.player_id} || X <- mnesia:table(city)])).

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
    [{unit_type, 1, "Footsolider", 1, 1, 2, 1, 5, 10}].

reset_game_tables() ->
    mnesia:clear_table(player),
    F = fun() ->
		foreach(fun mnesia:write/1, game_tables())
	end,    
    mnesia:transaction(F).    

%% Testing

example_tables() ->
    [
     {player, 1, <<"test">>, <<"123123">>, 0, false, [1,2], none},
     {player, 2, <<"test2">>, <<"123123">>, 0, false, [3], none},
     {player, 3, <<"test3">>, <<"123123">>, 0, false, [4], none},
     {player, 4, <<"test4">>, <<"123123">>, 0, false, [5], none},
     {connection, 1, none, none},
     {connection, 2, none, none},
     {connection, 3, none, none},
     {connection, 4, none, none},
     {city, 1, 1, 3, 4, 0},
     {city, 2, 2, 1, 5, 0},
     {city, 3, 3, 7, 4, 0},
     {army, 1, 1, 2,  2, 0, 0, none, 0, [1], [1]},
     {army, 2, 1, 5,  5, 0, 0, none, 0, none, none},
     {army, 3, 2, 8,  2, 0, 0, none, 0, none, none},
     {army, 4, 3, 10,10, 0, 0, none, 0, none, none},
     {army, 5, 4, 15, 2, 0, 0, none, 0, none, none},
     {army, 6, 5, 25,25, 0, 0, none, 0, none, none},
     {hero, 1, 1, 1},
     {unit, 1, 1, 1, 100, 1},
     {unit, 2, 2, 1, 10, 1},
     {unit, 3, 5, 1, 50, 1},
     {unit, 4, 6, 1, 25, 1}
    ].

reset_tables() ->
    mnesia:clear_table(player),
    F = fun() ->
		foreach(fun mnesia:write/1, example_tables())
	end,    
    mnesia:transaction(F).

