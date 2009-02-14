%% Author: Peter
%% Created: Dec 27, 2008
%% Description: TODO: Add description to game_loop
-module(game_loop).

%%
%% Include files
%%

-include("game.hrl").

%%
%% Exported Functions
%%
-export([loop/3]).

%%
%% API Functions
%%

loop(Iteration, CurrentTick, GamePID) ->            
	EntityList = gen_server:call(GamePID, 'GET_ENTITIES'),
	ArmyList = gen_server:call(GamePID, 'GET_ARMIES'),

    %Process character actions
    %process_actions(CharacterList),
    
    %Build simple perception
	perceptions(EntityList, ArmyList),
    %{MegaSec, Sec, MicroSec} = erlang:now(),
    CurrentMS = util:now_to_milliseconds(erlang:now()),
    NextTick = CurrentTick + 50,
	SleepTime = NextTick - CurrentMS,
    %io:fwrite("SleepTime: ~w~n", [SleepTime]),
    %io:fwrite("SleepTime: ~w : CurrentMicroseconds: ~w : CurrentTick ~w~n", [SleepTime, CurrentMS, CurrentTick]),
	%io:fwrite("PlayerList: ~w~n", [PlayerList]),
    timer:sleep(SleepTime),
	loop(Iteration + 1, NextTick, GamePID).
%%
%% Local Functions
%%

process_actions([]) ->
    ok;

process_actions(CharacterList) ->
    [CharPID | Rest] = CharacterList,
    
    Action = gen_server:call(CharPID, {'GET_ACTION'}),
    
    case Action of 
        move_north ->
            gen_server:cast(CharPID, {'MOVE', north});
        move_south ->
        	gen_server:cast(CharPID, {'MOVE', south});
        move_west ->
            gen_server:cast(CharPID, {'MOVE', west});
        move_east ->
            gen_server:cast(CharPID, {'MOVE', east});        
        none ->
     		ok
    end,    
    
    process_actions(Rest).


perceptions([], []) ->
    ok;

perceptions([], _) ->
    ok;
  
perceptions(EntityList, ArmyList) ->
    [Entity | Rest] = EntityList,   
    
    build_perception(Entity, ArmyList, []),
    
    perceptions(Rest, ArmyList).
    
build_perception(Entity, [], Perception) ->
    PlayerId = Entity#entity.player_id,
    gen_server:cast(global:whereis_name({player, PlayerId}), {'SEND_PERCEPTION', Perception});    

build_perception(Entity, ArmyList, Perception) ->
	
	EntityX = Entity#entity.x,
	EntityY = Entity#entity.y,
        
    [Army | Rest] = ArmyList,
	{ArmyID, ArmyX, ArmyY, State} = Army,
   
    DiffX = EntityX - ArmyX,
    DiffY = EntityY - ArmyY,
    
    Diff = (DiffX * DiffX) + (DiffY * DiffY),
       
    if
        Diff < 25 ->
			
			NewPerception = [{ArmyID, State, ArmyX, ArmyY} | Perception];
		true ->
            NewPerception = Perception,
			ok
	end,        
    
    build_perception(Entity, Rest, NewPerception). 
	                                                
                                             
  