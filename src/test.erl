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
	gen_tcp:send(Socket, <<?CMD_LOGIN, 4:16, "test", 6:16, "123123">>),
	loop(Socket).

%%
%% Local Functions
%%

loop(Socket) ->
	receive
		{tcp, Socket, Bin} ->
			io:fwrite("Bin: ~w~n", [Bin]),		
			case Bin of
				<<?CMD_PLAYER_ID, _PlayerId:32>> ->
					io:fwrite("Sending ?CMD_CLIENTREADY.~n"),		
					ok = gen_tcp:send(Socket, <<?CMD_CLIENTREADY>>),
					loop(Socket);
				<<?CMD_PERCEPTION, Perception/binary>> ->
					io:fwrite("Perception: ~w~n",[Perception]),
					loop(Socket);
				_Any ->
					io:fwrite("Do not recognize command.~n")
			end;							
		{error, closed} ->
			io:fwrite("Connection closed.")
	end.