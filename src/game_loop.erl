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
    PlayerList = gen_server:call(GamePID, 'GET_PLAYERS'),
    
    %Process events
    process_events(GamePID, CurrentTick, EventList),
    
    %Build simple perception
	%perceptions(EntityList, ObjectList),

    %Send perceptions
    send_perceptions(PlayerList),
    
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
    %io:fwrite("game_loop - EventList: ~w CurrentTick: ~w~n", [EventList, CurrentTick]),
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




perceptions([], []) ->
    ok;

perceptions([], _) ->
    ok;
  
perceptions(EntityList, ObjectList) ->
    [EntityPid | Rest] = EntityList,   
    
	EntityState = gen_server:call(EntityPid, {'GET_STATE'}),
	
    build_perception({EntityState#state.x, EntityState#state.y, EntityState#state.player_id}, ObjectList, []),
    
    perceptions(Rest, ObjectList).
    
build_perception(EntityInfo, [], Perception) ->
    {_, _, EntityPlayerId} = EntityInfo,
    gen_server:cast(global:whereis_name({player, EntityPlayerId}), {'ADD_PERCEPTION', Perception});    

build_perception(EntityInfo, ObjectList, Perception) ->
    {EntityX, EntityY, _} = EntityInfo,    
    
    [ObjectPid | Rest] = ObjectList,
	State = gen_server:call(ObjectPid, {'GET_STATE'}),
   
    DiffX = EntityX - State#state.x,
    DiffY = EntityY - State#state.y,
    
    Diff = (DiffX * DiffX) + (DiffY * DiffY),
       
    if
        Diff < 50 ->			
			NewPerception = [{State#state.id, 
							  State#state.player_id, 
							  State#state.type, 
							  State#state.state, 
							  State#state.x, 
							  State#state.y} | Perception];
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
  