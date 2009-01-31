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
	PlayerList = gen_server:call(GamePID, 'GET_PLAYER_LIST'),
	CharacterList = gen_server:call(GamePID, 'GET_CHAR_LIST'),

    %Process character actions
    process_actions(CharacterList),
    
    %Build simple perception
	perceptions(PlayerList, CharacterList),
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
  
perceptions(PlayerList, CharacterList) ->
    [Player | Rest] = PlayerList,   
    
    build_perception(Player, CharacterList, []),
    
    perceptions(Rest, CharacterList).
    
build_perception(Player, [], Perception) ->
    PlayerPID = Player#player_process.process,
    
    gen_server:cast(PlayerPID, {'SEND_PERCEPTION', Perception});    

build_perception(Player, CharacterList, Perception) ->
    PlayerId = Player#player_process.player_id,
    
    PlayerCharX = gen_server:call(global:whereis_name({character, PlayerId}), {'GET_X'}),
    PlayerCharY = gen_server:call(global:whereis_name({character, PlayerId}), {'GET_Y'}),
    
    [CharPID | Rest] = CharacterList,
       
    CharX = gen_server:call(CharPID, {'GET_X'}),
    CharY = gen_server:call(CharPID, {'GET_Y'}),
    
    DiffX = PlayerCharX - CharX,
    DiffY = PlayerCharY - CharY,
    
    Diff = (DiffX * DiffX) + (DiffY * DiffY),
       
    if
        Diff < 100 ->
            TargetPlayerId = gen_server:call(CharPID, {'GET_PLAYER_ID'}),
            TargetPlayerAction = gen_server:call(CharPID, {'GET_ACTION_ID'}),
			NewPerception = [{TargetPlayerId, CharX, CharY, TargetPlayerAction} | Perception];
		true ->
            NewPerception = Perception,
			ok
	end,        
    
    build_perception(Player, Rest, NewPerception). 
	                                                
                                             
  