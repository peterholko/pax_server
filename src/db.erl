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
-export([create_schema/0, start/0, write/1, read/2, delete/2, index_read/3, reset_tables/0,
        select_armies/0,
		select_cities/0,
		select_army/1,
		select_city/1]).

%%
%% API Functions
%%

create_schema() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    {atomic, ok} = mnesia:create_table(player,   [{attributes, record_info(fields, player)}]),
    {atomic, ok} = mnesia:create_table(connection,   [{attributes, record_info(fields, connection)}]),
    {atomic, ok} = mnesia:create_table(army,   [{attributes, record_info(fields, army)}]),
	{atomic, ok} = mnesia:create_table(city, [{attributes, record_info(fields, city)}]),
    {atomic, ok} = mnesia:create_table(explored_map, 
                                       [{disc_copies, [node()]},
                                       	{type, bag},
                                       	{attributes, record_info(fields, explored_map)}]),
    
    mnesia:add_table_index(player, name),
    mnesia:stop().

start() ->
    mnesia:start(),
    mnesia:wait_for_tables([player], 5000).

select(Name, Password) ->
    do(qlc:q([X ||	X <- mnesia:table(player),
             	 	X#player.name =:= Name,
                    X#player.password =:= Password
            		])).

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
    do(qlc:q([{X#army.id, X#army.x, X#army.y, X#army.state} || X <- mnesia:table(army)])).

select_cities() ->
	do(qlc:q([{X#city.id, X#city.x, X#city.y, X#city.state} || X <- mnesia:table(city)])).

select_army(PlayerId) ->
    do(qlc:q([#entity{id = X#army.id, x = X#army.x, y = X#army.y, player_id = PlayerId} || 	X <- mnesia:table(army),
              																				X#army.player_id =:= PlayerId])).

select_city(PlayerId) ->
    do(qlc:q([#entity{id = X#city.id, x = X#city.x, y = X#city.y, player_id = PlayerId} || 	X <- mnesia:table(city),
              																				X#city.player_id =:= PlayerId])).

select_explored_map(PlayerId) ->
    do(qlc:q([{X#explored_map.block_x, X#explored_map.block_y} || 	X <- mnesia:table(explored_map),
              														X#explored_map.player_id =:= PlayerId])).    

%%
%% Local Functions
%%

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

%% Testing

example_tables() ->
    [
     {player, 1, <<"test">>, <<"123123">>, 0, false, {1}, none},
     {player, 2, <<"test2">>, <<"123123">>, 0, false, {2}, none},
     {player, 3, <<"test3">>, <<"123123">>, 0, false, {3}, none},
     {player, 4, <<"test4">>, <<"123123">>, 0, false, {4}, none},
     {connection, 1, none, none},
     {connection, 2, none, none},
     {connection, 3, none, none},
     {connection, 4, none, none},
     {army, 1, 1, 2,  2, none, none, none},
     {army, 2, 1, 5,  5, none, none, none},
     {army, 2, 2, 15, 15, none, none, none},
     {army, 3, 3, 10, 10, none, none, none},
     {army, 4, 4, 15, 2, none, none, none},
     {army, 5, 5, 25, 25, none, none, none},
     {explored_map, 1, 5, 5},
     {explored_map, 1, 10, 10}
    ].

reset_tables() ->
    mnesia:clear_table(player),
    F = fun() ->
		foreach(fun mnesia:write/1, example_tables())
	end,    
    mnesia:transaction(F).
