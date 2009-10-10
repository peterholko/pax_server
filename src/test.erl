%% Author: Peter
%% Created: Apr 2, 2009
%% Description: TODO: Add description to test
-module(test).

%%
%% Include files
%%

-include("packet.hrl").

%%
%% Exported Functions
%%
-export([run/0]).

%%
%% API Functions
%%

run() ->
	{ok,Sock} = gen_tcp:connect("localhost",2345,[{active,false},
                                                    {packet,0}]),
    gen_tcp:send(Sock,?CMD_POLICYREQUEST),
    Policy = gen_tcp:recv(Sock,0),
	io:fwrite("Test @@ ~w~n", [Policy]),
	gen_tcp:send(Sock, <<?CMD_LOGIN, "test", "123123">>),
	Login = gen_tcp:recv(Sock,0),
	io:fwrite("Test @@ ~w~n", [Login]),
    gen_tcp:close(Sock).

%%
%% Local Functions
%%

