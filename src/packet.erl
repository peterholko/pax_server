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
-export([read/1, send/2, send_policy/1, send_clocksync/1]).

%%
%% Local Functions
%%

name() ->
    string().

pass() ->
    string().

player() ->
    int().

id() ->
    int().

state() ->
    short().

type() ->
    short().

type_name() -> 
    list(short(), byte()).

size() ->
	int().

x() ->
    short().

y() ->
    short().

level() ->
	short().

entity() ->
    tuple({id(), player(), type(), state(), x(), y()}).

entities() ->
    list(short(), entity()).

tile() ->
    tuple({int(), byte()}).

tiles() ->
	list(int(), tile()). 

info_list() ->
    list(int(), string()).    

hero() ->
	id().

unit() ->
	tuple({id(), type_name(), size()}).

units() ->
	list(short(), unit()).

buildings() ->
    list(short(), int()).

building_type() ->
    byte().

queue_unit_amount() ->
    int().

start_time() ->
    int().

build_time() ->
    int().

queue_unit() ->
    tuple({id(), queue_unit_amount(), start_time(), build_time()}).

queue_units() ->
    list(building_type(), queue_unit()).

% packet records

login() ->
    record(login, {name(),
                   pass()}).

move() ->
    record(move, {id(),
                  x(),
                  y()}).

bad() ->
    record(bad, {byte(),
                 byte()}).

player_id() ->
    record(player_id, {player()}).

perception() ->
    record(perception, {entities(),
                        tiles()}).

map() ->
    record(map, {tiles()}).

attack() ->
    record(attack, {id(),
                    id()}).

request_info() ->
    record(request_info, {short(),
                          id()}).

info() ->
    record(info, {info_list()}).

info_army() ->
	record(info_army, {hero(),
					   units()}).

info_city() ->
    record(info_city, {buildings()}).

info_unit_queue() ->
    record(info_unit_queue, {building_type(),
                             queue_units()}).

%%
%% API Functions
%%

read(?CMD_POLICYREQUEST) ->
	io:fwrite("packet: CMD_POLICYREQUEST.~n"),
    policy_request;

read(<<?CMD_LOGIN, Bin/binary>>) ->
	io:fwrite("packet: read() - Read Data accepted: ~w~n", [Bin]),
    unpickle(login(), Bin);

read(<<?CMD_CLOCKSYNC>>) ->
	io:fwrite("packet: read() - clocksync~n"),
	clocksync;

read(<<?CMD_CLIENTREADY>>) ->
	io:fwrite("packet: read() - clientready~n"),
	clientready;

read(<<?CMD_MOVE, Bin/binary>>) ->
	unpickle(move(), Bin);

read(<<?CMD_ATTACK, Bin/binary>>) ->
	unpickle(attack(), Bin);

read(<<?CMD_REQUEST_INFO, Bin/binary>>) ->
	unpickle(request_info(), Bin).

write(R) when is_record(R, bad) ->
    [?CMD_BAD|pickle(bad(), R)];

write(R) when is_record(R, player_id) ->
    [?CMD_PLAYER_ID|pickle(player_id(), R)];

write(R) when is_record(R, perception) ->
    [?CMD_PERCEPTION|pickle(perception(), R)];

write(R) when is_record(R, map) ->
    [?CMD_EXPLORED_MAP|pickle(map(), R)];

write(R) when is_record(R, info) ->
    [?CMD_INFO|pickle(info(), R)];

write(R) when is_record(R, info_army) ->
	[?CMD_INFO_ARMY|pickle(info_army(), R)];

write(R) when is_record(R, info_city) ->
	[?CMD_INFO_CITY|pickle(info_city(), R)];

write(R) when is_record(R, info_unit_queue) ->
    [?CMD_INFO_UNIT_QUEUE|pickle(info_unit_queue(), R)].

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

