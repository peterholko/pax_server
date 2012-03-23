%% Author: Peter
%% Created: Dec 20, 2008
%% Description: TODO: Add description to login
-module(login).

%%
%% Include files
%%

-include("packet.hrl").
-include("schema.hrl").

%%
%% Exported Functions
%%
-export([login/3]).

%% 
%% API Functions
%%

login(Name, Pass, Socket) 
  when is_binary(Name),
       is_binary(Pass),
       is_pid(Socket) -> % socket handler process
    io:fwrite("login:login - Name: ~w~n", [Name]),
    io:fwrite("login:login - Pass: ~w~n", [Pass]),
    
    PlayerInfo = db:index_read(player, Name, #player.name),
    io:fwrite("login:login - PlayerInfo: ~w~n", [PlayerInfo]),
    login(PlayerInfo, [Name, Pass, Socket]).

%%
%% Local Functions
%%

login([], _) ->
    %% player not found
    io:fwrite("login:login - account not found. ~n"),
    {error, ?ERR_BAD_LOGIN};

login([PlayerInfo], [_Name, Pass,_] = Args)
  when is_record(PlayerInfo, player) ->
    io:fwrite("login:login - PlayerInfo2: ~w~n", [PlayerInfo]),
    PlayerId = PlayerInfo#player.id,
    PlayerConn = case db:read(connection, PlayerId) of
                     [P] ->
                         P;
                     _ ->
                         ok = db:delete(connection, PlayerId),
                         #connection{ player_id = PlayerId }
                 end,    
    %% replace dead ids with none
    PlayerConn1 = PlayerConn#connection {
                                         socket = fix_pid(PlayerConn#connection.socket),
                                         process = fix_pid(PlayerConn#connection.process)
                                        },
    %% check player state and login
    Condition = check_player(PlayerInfo, PlayerConn1, [Pass], 
                             [
                              fun is_account_disabled/3,
                              fun is_bad_password/3,
                              %fun is_player_busy/3,
                              fun is_player_online/3,
                              %fun is_client_down/3,
                              fun is_offline/3
                             ]),    
    
    {Player2, PlayerInfo1, Result} = login(PlayerInfo, PlayerConn1, Condition, Args),
    case {db:write(Player2), db:write(PlayerInfo1)} of
        {ok, ok} ->
            io:fwrite("login: login - Result: ~w~n", [Result]),
            Result;
        _ ->
            {error, ?ERR_UNKNOWN}
    end.  

login(PlayerInfo, PlayerConn, account_disabled, _) ->
    {PlayerInfo, PlayerConn, {error, ?ERR_ACCOUNT_DISABLED}};

login(PlayerInfo, PlayerConn, player_online, Args) ->
    %% player is already online
    io:fwrite("login: player is already online~n"),
    ok = gen_server:call(PlayerConn#connection.process, 'LOGOUT'),
    timer:sleep(100),
    login(PlayerInfo, PlayerConn, player_offline, Args);

login(PlayerInfo, PlayerConn, bad_password, _) ->
    N = PlayerInfo#player.login_errors + 1,
    %[CC] = db:read(tab_cluster_config, 0),
    MaxLoginErrors = 10,
    if
        N > MaxLoginErrors ->
            %% disable account
            PlayerInfo1 = PlayerInfo#player { disabled = true },
            {PlayerInfo1, PlayerConn, {error, ?ERR_ACCOUNT_DISABLED}};
        true ->
            PlayerInfo1 = PlayerInfo#player { login_errors = N },
            {PlayerInfo1, PlayerConn, {error, ?ERR_BAD_LOGIN}}
    end;

login(PlayerInfo, PlayerConn, player_offline, [Name, _, Socket]) ->
    %% start player process
    {ok, Pid} = player:start(Name),

    player:set_socket(Pid, Socket),
    chat:add(PlayerInfo#player.id, Name, Socket),

    %% update player connection
    PlayerConn1 = PlayerConn#connection {
                                         player_id = PlayerInfo#player.id,
                                         process = Pid,
                                         socket = Socket
                                        },
    {PlayerInfo, PlayerConn1, {ok, Pid}}.

check_player(PlayerInfo, PlayerConn, Pass, [Guard|Rest]) ->
    case Guard(PlayerInfo, PlayerConn, Pass) of
        {true, Condition} ->
            Condition;
        _ ->
            check_player(PlayerInfo, PlayerConn, Pass, Rest)
    end;

check_player(_Info, _PlayerConn, _Args, []) ->
    %% fall through
    unknown_error.

is_account_disabled(PlayerInfo, _, _) ->
    {PlayerInfo#player.disabled, account_disabled}.

is_player_online(_, PlayerConn, _) ->
    SocketAlive = PlayerConn#connection.socket /= none,
    PlayerAlive = PlayerConn#connection.process /= none,
    {SocketAlive and PlayerAlive, player_online}.

is_bad_password(PlayerInfo, _, [Pass]) ->
    %Hash = erlang:phash2(Pass, 1 bsl 32),
    Match = PlayerInfo#player.password =:= Pass,
    io:fwrite("login: is_bad_password - Match: ~w~n", [Match]),
    {not Match, bad_password}.

is_offline(_, PlayerConn, _) ->
    SocketDown = PlayerConn#connection.socket =:= none,
    PlayerDown = PlayerConn#connection.process =:= none,
    {SocketDown and PlayerDown, player_offline}.    



fix_pid(none) ->
    none;

fix_pid(Pid)
  when is_pid(Pid) ->
    case util:is_process_alive(Pid) of
        true ->
            Pid;
        _ ->
            none
    end.
