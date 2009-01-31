%% Author: Peter
%% Created: Dec 15, 2008
%% Description: TODO: Add description to db
-module(db).

%%
%% Include files
%%
-import(lists, [reverse/1, foreach/2]).

-include("schema.hrl").

-include_lib("stdlib/include/qlc.hrl").

%%
%% Exported Functions
%%
-export([create_schema/0, start/0, select/2, write/1, read/2, delete/2, index_read/3, reset_tables/0]).

%%
%% API Functions
%%

create_schema() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(player,   [{attributes, record_info(fields, player)}]),
    mnesia:create_table(connection,   [{attributes, record_info(fields, connection)}]),
    mnesia:create_table(character,   [{attributes, record_info(fields, character)}]),
    
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

test_account() ->
    F = fun() ->  mnesia:index_read(player, <<"test">>, #player.name) end,
    {atomic, Value} = mnesia:transaction(F),
	Value.
 

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
     {player, 1, <<"test">>, <<"123123">>, 0, false, 1},
     {player, 2, <<"test2">>, <<"123123">>, 0, false, 2},
     {player, 3, <<"test3">>, <<"123123">>, 0, false, 3},
     {player, 4, <<"test4">>, <<"123123">>, 0, false, 4},
     {connection, 1, none, none},
     {connection, 2, none, none},
     {connection, 3, none, none},
     {connection, 4, none, none},
     {character, 1, 5,5,0, none},
     {character, 2, 10,10,0, none},
     {character, 3, 5,10,0, none},
     {character, 4, 10,5,0, none},
     {character, 5, 20,20,0, none}
    ].

reset_tables() ->
    mnesia:clear_table(player),
    F = fun() ->
		foreach(fun mnesia:write/1, example_tables())
	end,    
    mnesia:transaction(F).