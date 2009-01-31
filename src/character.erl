%% Author: Peter
%% Created: Dec 26, 2008
%% Description: TODO: Add description to character
-module(character).
-behaviour(gen_server).

%%
%% Include files
%%

-include("packet.hrl").
-include("schema.hrl").

%%
%% Exported Functions
%%
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-export([start/1, stop/1]).

-record(module_data, {
          player_id,   
          character_info,
          self
         }).

%%
%% API Functions
%%

start(PlayerId) ->

    case db:read(character, PlayerId) of
        [CharacterInfo] ->
            gen_server:start({global, {character, PlayerId}}, character, [PlayerId, CharacterInfo], []);
        Any ->
            {error, Any}
    end.

init([ID, CharacterInfo]) 
  when is_integer(ID) ->
    process_flag(trap_exit, true),
    {ok, #module_data{ player_id = ID, character_info = CharacterInfo, self = self() }}.

terminate(_Reason, Data) ->
    ok.

stop(ProcessId) 
  when is_pid(ProcessId) ->
    gen_server:cast(ProcessId, stop).

handle_cast({'SET_ACTION', Action}, Data) ->
   	CharacterInfo = Data#module_data.character_info,
    NewCharacterInfo = CharacterInfo#character { action = Action},
    NewData = Data#module_data{ character_info = NewCharacterInfo },
    {noreply, NewData};

handle_cast({'MOVE', Direction}, Data) ->
    CharacterInfo = Data#module_data.character_info,
    X = CharacterInfo#character.x,
    Y = CharacterInfo#character.y,
    
    	case Direction of
            north ->
                NewX = X,
                NewY = Y - 1;
           	south ->
                NewX = X,
                NewY = Y + 1;
            west ->
                NewX = X - 1,
                NewY = Y;
            east ->
                NewX = X + 1,
                NewY = Y
    	end,
            
    NewCharacterInfo = CharacterInfo#character { x = NewX, y = NewY},
    NewData = Data#module_data{ character_info = NewCharacterInfo },
    {noreply, NewData};

handle_cast({'SET_X', X}, Data) ->
   	CharacterInfo = Data#module_data.character_info,
    NewCharacterInfo = CharacterInfo#character { x = X},
    NewData = Data#module_data{ character_info = NewCharacterInfo },
    {noreply, NewData};

handle_cast({'SET_Y', Y}, Data) ->
   	CharacterInfo = Data#module_data.character_info,
    NewCharacterInfo = CharacterInfo#character { y = Y},
    NewData = Data#module_data{ character_info = NewCharacterInfo },
    {noreply, NewData};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({'GET_X'}, _From, Data) ->
    %io:fwrite("character: GET_X ~n"),  
	CharacterInfo = Data#module_data.character_info,
    X = CharacterInfo#character.x,    
	{reply, X, Data};

handle_call({'GET_Y'}, _From, Data) ->
    %io:fwrite("character: GET_Y ~n"),  
	CharacterInfo = Data#module_data.character_info,
    Y = CharacterInfo#character.y,    
	{reply, Y, Data};

handle_call({'GET_ACTION'}, _From, Data) ->
	CharacterInfo = Data#module_data.character_info,
    Action = CharacterInfo#character.action,     
    {reply, Action, Data};

handle_call({'GET_ACTION_ID'}, _From, Data) ->
	CharacterInfo = Data#module_data.character_info,
    Action = CharacterInfo#character.action,     
    ActionId = get_action_id(Action),
    {reply, ActionId, Data};

handle_call({'GET_PLAYER_ID'}, _From, Data) ->
	{reply, Data#module_data.player_id, Data};

handle_call(Event, From, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
                              {line, ?LINE},
                              {self, self()}, 
                              {message, Event}, 
                              {from, From}
                             ]),
    {noreply, Data}.

handle_info(Info, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
                              {line, ?LINE},
                              {self, self()}, 
                              {message, Info}]),
    {noreply, Data}.

code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.
%%
%% Local Functions
%%

get_action_id(Action) ->
        
    case Action of
        none ->
            ActionId = 0;
        move_north ->
            ActionId = 1;
        move_east ->
            ActionId = 2;
        move_south ->
            ActionId = 3;
        move_west ->
            ActionId = 4;
    	_ -> 
            ActionId = 0
    end,
    
    ActionId.

            

