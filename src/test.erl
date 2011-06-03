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
-export([run/1, market/0]).

%%
%% API Functions
%%

run(Account) ->
    spawn(fun() -> connect(Account) end).

connect(Account) ->
    {ok,Socket} = gen_tcp:connect("localhost",2345,[binary,{active, true},{nodelay, true}, {keepalive, true}, {packet,0}]),
    t:start(Socket), 
    t:login(Account),   

    loop(Socket).

market() ->
    t:add_claim(),
    timer:sleep(1000),
    t:build_farm(),
    timer:sleep(1000),
    t:assign_task().


%%
%% Local Functions
%%

loop(Socket) ->
    receive
        {tcp, Socket, Bin} ->		
            io:fwrite("Test - Bin: ~w~n", [Bin]),
            case packet:read(Bin) of 
                #player_id{id = PlayerId} ->
                    io:fwrite("PlayerId: ~w~n", [PlayerId]),		
                    ok = gen_tcp:send(Socket, <<?CMD_CLIENTREADY>>),
                    loop(Socket);
                #info_kingdom{id = Id, name = Name, gold = Gold} ->
                    io:fwrite("KingdomId: ~w Name: ~w Gold: ~w~n", [Id, Name, Gold]),
                    loop(Socket);
                #map{tiles = Tiles } ->
                    io:fwrite("Tiles: ~w~n", [Tiles]),
                    loop(Socket);
                #perception{entities = Entities, tiles = Tiles} ->
                    io:fwrite("Perception: ~w ~w~n",[Entities, Tiles]),
                    loop(Socket);                
                #info_army{id = Id, units = Units} ->
                    io:fwrite("Info Army: ~w ~w~n", [Id, Units]),
                    loop(Socket);
                #info_city{id = Id, 
                           buildings = Buildings, 
                           units = Units,
                           claims = Claims,
                           improvements = Improvements,
                           assignments = Assignments,
                           items = Items,
                           populations = Populations  
                           } ->
                    io:fwrite("Info City: ~w~n ~w~n ~w~n ~w~n ~w~n ~w~n ~w~n ~w~n", 
                               [Id, Buildings, Units, Claims, Assignments, Improvements, Items, Populations]),
                    loop(Socket);
                #battle_info{battle_id = BattleId, armies = Armies} ->
                    io:fwrite("Battle Info: ~w ~w~n", [BattleId, Armies]),
                    loop(Socket);
                #battle_add_army{battle_id = BattleId, army = Army} ->
                    io:fwrite("Battle Add Army: ~w ~w~n", [BattleId, Army]),
                    loop(Socket);
                #battle_damage{battle_id = BattleId, source_id = SourceId, target_id = TargetId, damage = Damage} ->
                    io:fwrite("Battle Damage: ~w ~w ~w ~w~n", [BattleId, SourceId, TargetId, Damage]),
                    loop(Socket);                
                _Any ->
                    io:fwrite("Do not recognize command.~nBin: ~w~n", [Bin]),
                    loop(Socket)
            end;							
        {error, closed} ->
            io:fwrite("Connection closed.");
        _Any ->
            io:fwrite("Epic failure.")
    end.
