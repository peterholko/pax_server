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

unit_id() ->
    id().

unit_size() ->
	int().

unit_type() ->
    short().

start_time() ->
    int().

end_time() ->
    int().

unit() ->
	tuple({unit_id(), unit_type(), unit_size()}).

unit_queue() ->
    tuple({unit_id(), unit_type(), unit_size(), start_time(), end_time()}).

units() ->
	list(short(), unit()).

buildings() ->
    list(short(), int()).

units_queue() ->
    list(short(), unit_queue()).

source_id() ->
    id().

source_type() ->
    type().

target_id() ->
    id().

target_type() ->
    type().


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
	record(info_army, {id(),
                       hero(),
					   units()}).

info_city() ->
    record(info_city, {id(),
                       buildings(),
                       units(),
                       units_queue()}).

city_queue_unit() ->
    record(city_queue_unit, {id(),
                             unit_type(),
                             unit_size()}).

transfer_unit() ->
    record(transfer_unit, {unit_id(),
                           source_id(),
                           source_type(),
                           target_id(),
                           target_type()}).

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
	unpickle(request_info(), Bin);

read(<<?CMD_CITY_QUEUE_UNIT, Bin/binary>>) ->
	unpickle(city_queue_unit(), Bin);

read(<<?CMD_TRANSFER_UNIT, Bin/binary>>) ->
	unpickle(transfer_unit(), Bin).

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
	[?CMD_INFO_CITY|pickle(info_city(), R)].

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

