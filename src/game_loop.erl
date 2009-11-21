%% Author: Peter
%% Created: Dec 27, 2008
%% Description: TODO: Add description to game_loop
-module(game_loop).

%%
%% Include files
%%

-include("common.hrl").
-include("game.hrl").

%%
%% Exported Functions
%%
-export([loop/2]).

%%
%% API Functions
%%

loop(LastTime, GamePID) ->
    
    CurrentTick = gen_server:call(GamePID, 'GET_TICK'), 	
    EventList = gen_server:call(GamePID, 'GET_EVENTS'),
    UpdatePerceptions = gen_server:call(GamePID, 'GET_UPDATE_PERCEPTION'),
    
    %Process events
    process_events(GamePID, CurrentTick, EventList),
    
    %Build simple perception
    %perceptions(EntityList, ObjectList),
    
    %Send perceptions
    send_perceptions(UpdatePerceptions),
    clear_perceptions(GamePID),
    
    CurrentTime = util:get_time(),
    NextTime = LastTime + ?GAME_LOOP_TICK,
    CalcSleepTime = NextTime - CurrentTime,
    
    gen_server:cast(GamePID, 'NEXT_TICK'),
    
    if
        CalcSleepTime =< 0 ->
            io:fwrite("loop - SleepTime: ~w~n", [CalcSleepTime]),
            SleepTime = 1;
        true ->
            SleepTime = CalcSleepTime            
    end,
    
    timer:sleep(SleepTime),
    loop(NextTime, GamePID).
%%
%% Local Functions
%%

process_events(_, _, []) ->
    ok;

process_events(GamePID, CurrentTick, EventList) ->
    [Event | Rest] = EventList,
    {EventId, Pid, Type, EventData, EventTick} = Event,
    
    if
        CurrentTick =:= EventTick ->
            gen_server:cast(Pid, {'PROCESS_EVENT', EventData, Type}),
            gen_server:cast(GamePID, {'DELETE_EVENT', EventId});
        true ->
            ok
    end,
    
    process_events(GamePID, CurrentTick, Rest).

send_perceptions([]) ->
    ok;

send_perceptions(UpdatePerception) ->
    [PlayerId | Rest] = UpdatePerception,
        
    PlayerPid = global:whereis_name({player,PlayerId}),       
    case is_pid(PlayerPid) of
        true -> gen_server:cast(PlayerPid, {'SEND_PERCEPTION'});
        false -> ok
    end,
    
    send_perceptions(Rest).

clear_perceptions(GamePID) ->
    gen_server:cast(GamePID, 'CLEAR_PERCEPTIONS').
