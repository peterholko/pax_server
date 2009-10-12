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
	{ok,Socket} = gen_tcp:connect("localhost",2345,[binary,{active, once}, {keepalive, true}, {packet,0}]),
    ok = gen_tcp:send(Socket,?CMD_POLICYREQUEST),
	loop(Socket).
    %Policy = gen_tcp:recv(Sock,0),
	%io:fwrite("Test @@ ~w~n", [Policy]),
	%gen_tcp:send(Sock, <<?CMD_LOGIN, 4:16, "test", 6:16, "123123">>),
	%Login = gen_tcp:recv(Sock,0),
	%io:fwrite("Test @@ ~w~n", [Login]),

%%
%% Local Functions
%%

loop(Socket) ->
	receive
		{tcp, Socket, Bin} ->
			io:fwrite("Bin: ~w~n", [Bin]),		
			case Bin of
				?CMD_POLICY ->
					timer:sleep(2000),
					io:fwrite("Sending ?CMD_LOGIN.~n"),						
					%{ok, NewSocket} = gen_tcp:connect("localhost",2345,[binary,{packet,0}]),
					Status = gen_tcp:send(Socket, <<?CMD_LOGIN, 4:16, "test", 6:16, "123123">>),
					io:fwrite("Send Status: ~w~n", [Status]),
 					inet:setopts(Socket,[{active, once}]),
					loop(Socket);
				<<?CMD_PLAYER_ID, _PlayerId:32>> ->
					timer:sleep(1000),
					io:fwrite("Sending ?CMD_CLIENTREADY.~n"),		
					%{ok, NewSocket} = gen_tcp:connect("localhost",2345,[binary,{packet,0}]),
					Status = gen_tcp:send(Socket, <<?CMD_CLIENTREADY>>),
					io:fwrite("Send Status: ~w~n", [Status]),
 					inet:setopts(Socket,[{active, once}]),
					loop(Socket);
				_Any ->
					io:fwrite("Do not recognize command.~n")
			end;							
		{error, closed} ->
			io:fwrite("Connection closed.")
	end.