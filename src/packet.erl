%% Author: Peter
%% Created: Dec 15, 2008
%% Description: TODO: Add description to packet
-module(packet).

%%
%% Include files
%%
-include("packet.hrl").

-import(pickle, [pickle/2, unpickle/2, byte/0, 
                 short/0, sshort/0, int/0, sint/0, 
                 long/0, slong/0, list/2, choice/2, 
                 optional/1, wrap/2, tuple/1, record/2, 
                 binary/1, string/0, wstring/0
                ]).

%%
%% Exported Functions
%%
-export([read/1, send/2, send_policy/1, send_clocksync/1, send_ping/1]).

%%
%% Local Functions
%%

name() ->
    string().

pass() ->
    string().

player() ->
    short().

x() ->
    short().

y() ->
    short().

action() ->
    short().

character() ->
    tuple({player(), x(), y(), action()}).

direction() ->
    byte().

tile() ->
    byte().

login() ->
    record(login, {
             name(),
             pass()
            }).

move() ->
    record(move, { direction() }).

bad() ->
    record(bad, {
             byte(),
             byte()
            }).

player_id() ->
    record(player_id, {
             player()
            }).

characters() ->
    list(byte(), character()).

perception() ->
    record(perception, {
                        characters()
                        }).

tiles() ->
    list(short(), byte()).

map() ->
    record(map, {
                 tiles()
                }).


%%
%% API Functions
%%

read(<<?CMD_LOGIN, Bin/binary>>) ->
	io:fwrite("packet: read() - Read Data accepted: ~w~n", [Bin]),
    unpickle(login(), Bin);

read(<<?CMD_MOVE, Bin/binary>>) ->
	unpickle(move(), Bin);

read(<<?CMD_PING>>) ->
	io:fwrite("packet: read() - clocksync~n"),
	ping;

read(<<?CMD_CLOCKSYNC>>) ->
	io:fwrite("packet: read() - clocksync~n"),
	clocksync;

read(?CMD_POLICYREQUEST) ->
	io:fwrite("packet: CMD_POLICYREQUEST.~n"),
    policy_request.

write(R) when is_record(R, bad) ->
    [?CMD_BAD|pickle(bad(), R)];

write(R) when is_record(R, player_id) ->
    [?CMD_PLAYER_ID|pickle(player_id(), R)];

write(R) when is_record(R, perception) ->
    [?CMD_PERCEPTION|pickle(perception(), R)];

write(R) when is_record(R, map) ->
    [?CMD_MAP|pickle(map(), R)].

send(Socket, Data) ->
    io:format("packet: send() - Data ->  ~p~n", [Data]),
    Bin = list_to_binary(write(Data)),
    io:format("packet: send() -  ~p~n", [Bin]),
    case catch gen_tcp:send(Socket, Bin) of
        ok ->
            ok;
        {error, closed} ->
            ok;
        {error,econnaborted} ->
            ok;
        Any ->
            error_logger:error_report([
                                       {message, "gen_tcp:send error"},
                                       {module, ?MODULE}, 
                                       {line, ?LINE},
                                       {socket, Socket}, 
                                       {port_info, erlang:port_info(Socket, connected)},
                                       {data, Data},
                                       {bin, Bin},
                                       {error, Any}
                                      ])
    end.

send_policy(Socket) ->
    Bin = ?CMD_POLICY,
    io:format("packet: send() -  ~p~n", [Bin]),
    case catch gen_tcp:send(Socket, Bin) of
        ok ->
            ok,
        	gen_tcp:close(Socket);
        {error, closed} ->
            ok;
        {error,econnaborted} ->
            ok;
        Any ->
            error_logger:error_report([
                                       {message, "gen_tcp:send_policy error"},
                                       {module, ?MODULE}, 
                                       {line, ?LINE},
                                       {socket, Socket}, 
                                       {port_info, erlang:port_info(Socket, connected)},
                                       {bin, Bin},
                                       {error, Any}
                                      ])
    end.

send_clocksync(Socket) ->
    {MegaSec, Sec, MicroSec} = erlang:now(),
    CurrentMS = (MegaSec * 1000000000) + (Sec * 1000) + (MicroSec div 1000), 
    io:format("packet: send_clocksync() -  ~p~n", [CurrentMS]),
    case catch gen_tcp:send(Socket, <<?CMD_CLOCKSYNC, CurrentMS:64>>) of
        ok ->
            ok;
        {error, closed} ->
            ok;
        {error,econnaborted} ->
            ok;
        Any ->
            error_logger:error_report([
                                       {message, "gen_tcp:send_clocksync error"},
                                       {module, ?MODULE}, 
                                       {line, ?LINE},
                                       {socket, Socket}, 
                                       {port_info, erlang:port_info(Socket, connected)},
                                       {error, Any}
                                      ])
    end.

send_ping(Socket) ->
    {MegaSec, Sec, MicroSec} = erlang:now(),
    CurrentMS = (MegaSec * 1000000000) + (Sec * 1000) + (MicroSec div 1000), 
    io:format("packet: send_ping() -  ~p~n", [CurrentMS]),
    case catch gen_tcp:send(Socket, <<?CMD_PING, CurrentMS:64>>) of
        ok ->
            ok;
        {error, closed} ->
            ok;
        {error,econnaborted} ->
            ok;
        Any ->
            error_logger:error_report([
                                       {message, "gen_tcp:send_ping error"},
                                       {module, ?MODULE}, 
                                       {line, ?LINE},
                                       {socket, Socket}, 
                                       {port_info, erlang:port_info(Socket, connected)},
                                       {error, Any}
                                      ])
    end.
