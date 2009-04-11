%% Author: Peter
%% Created: Apr 2, 2009
%% Description: TODO: Add description to test
-module(test).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start_db/0]).

%%
%% API Functions
%%

start_db() ->
    % Create schema and load db data
    io:fwrite("Creating schema and loading db data..."),
    db:create_schema(),
	db:start(),
	db:reset_game_tables(),
	db:reset_tables().


%%
%% Local Functions
%%

