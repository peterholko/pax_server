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
	EntityList = gen_server:call(GamePID, 'GET_ENTITIES'),    
	ObjectList = gen_server:call(GamePID, 'GET_OBJECTS'),
	EventList = gen_server:call(GamePID, 'GET_EVENTS'),
    PlayerList = gen_server:call(GamePID, 'GET_PLAYERS'),
    
    %Process events
    process_events(GamePID, CurrentTick, EventList),
    
    %Build simple perception
	perceptions(EntityList, ObjectList),

    %Send perceptions
    send_perceptions(PlayerList),
    
    CurrentTime = util:now_to_milliseconds(erlang:now()),
    NextTime = LastTime + ?GAME_LOOP,
	SleepTime = NextTime - CurrentTime,

    gen_server:cast(GamePID, 'NEXT_TICK'),
    %io:fwrite("loop - SleepTime: ~w~n", [SleepTime]),
    timer:sleep(SleepTime),
	loop(NextTime, GamePID).
%%
%% Local Functions
%%

process_events(_, _, []) ->
    ok;

process_events(GamePID, CurrentTick, EventList) ->
    io:fwrite("game_loop - EventList: ~w CurrentTick: ~w~n", [EventList, CurrentTick]),
    [Event | Rest] = EventList,
    {EventId, Pid, Type, EventTick} = Event,
    
    if
        CurrentTick =:= EventTick ->
            gen_server:cast(Pid, {'PROCESS_EVENT', Type}),
        	gen_server:cast(GamePID, {'DELETE_EVENT', EventId});
        true ->
            ok
    end,
    
    process_events(GamePID, CurrentTick, Rest).

perceptions([], []) ->
    ok;

perceptions([], _) ->
    ok;
  
perceptions(EntityList, ObjectList) ->
    [EntityPid | Rest] = EntityList,   
    
    {EntityId, 
     EntityPlayerId, 
     EntityType, 
     EntityState, 
     EntityX, 
     EntityY} = gen_server:call(EntityPid, {'GET_STATE'}),
    
    EntityPlayerId = gen_server:call(EntityPid, {'GET_PLAYER_ID'}),
    
    build_perception({EntityX, EntityY, EntityPlayerId}, ObjectList, []),
    
    perceptions(Rest, ObjectList).
    
build_perception(EntityInfo, [], Perception) ->
    {_, _, EntityPlayerId} = EntityInfo,
    gen_server:cast(global:whereis_name({player, EntityPlayerId}), {'ADD_PERCEPTION', Perception});    

build_perception(EntityInfo, ObjectList, Perception) ->
    {EntityX, EntityY, _} = EntityInfo,    
    
    [ObjectPid | Rest] = ObjectList,
	{Id, PlayerId, Type, State, X, Y} = gen_server:call(ObjectPid, {'GET_STATE'}),
   
    DiffX = EntityX - X,
    DiffY = EntityY - Y,
    
    Diff = (DiffX * DiffX) + (DiffY * DiffY),
       
    if
        Diff < 50 ->
			
			NewPerception = [{Id, PlayerId, Type, State, X, Y} | Perception];
		true ->
            NewPerception = Perception
	end,        

    build_perception(EntityInfo, Rest, NewPerception). 

send_perceptions([]) ->
    ok;

send_perceptions(PlayerList) ->
    [Player | Rest] = PlayerList,
    gen_server:cast(Player#player_process.process, {'SEND_PERCEPTION'}),
    send_perceptions(Rest).
  