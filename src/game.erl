%% Author: Peter
%% Created: Dec 25, 2008
%% Description: TODO: Add description to game
-module(game).
-behaviour(gen_server).

%%
%% Include files
%%

-include("game.hrl").

%%
%% Exported Functions
%%
-export([start/0, init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

%%
%% API Functions
%%

start() ->
	gen_server:start({global, game_pid}, game, [], []).

init([]) ->
    %% Load armies, cities
    Armies = db:select_armies(),
	Cities = db:select_cities(),
	Data = #game_info {armies = Armies, cities = Cities},
	{ok, Data}.

terminate(_Reason, Data) ->
    ok.

handle_cast(stop, Data) ->
    {stop, normal, Data};

handle_cast({'ADD_PLAYER', PlayerId, ProcessId, NewEntities}, Data) ->
	NewPlayer = #player_process{player_id = PlayerId, process = ProcessId},

   	EntityList = Data#game_info.entities,
	NewEntityList = entity:add_entities(EntityList, NewEntities),	

    PlayerList = Data#game_info.players,
	NewPlayerList = [NewPlayer|PlayerList],    
	
	NewData = Data#game_info {
            		players = NewPlayerList,
					entities = NewEntities
           			},
    {noreply, NewData};

handle_cast({'DELETE_PLAYER', PlayerId}, Data) ->
		
	NewPlayerList = lists:keydelete(PlayerId, 2, Data#game_info.players),
    io:fwrite("game - DELETE_PLAYER() PlayerId: ~w~n", [PlayerId]),
    io:fwrite("game - DELETE_PLAYER() NewPlayerList: ~w~n", [NewPlayerList]),
	NewData = Data#game_info {
            		players = NewPlayerList
           			},
	{noreply, NewData}.

handle_call('GET_PLAYERS', _From, Data) ->
	{reply, Data#game_info.players, Data};

handle_call('GET_ARMIES', _From, Data) ->
	{reply, Data#game_info.armies, Data};

handle_call('GET_ENTITIES', _From, Data) ->
	{reply, Data#game_info.entities, Data};

handle_call(Event, From, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
                              {line, ?LINE},
                              {self, self()}, 
                              {message, Event}, 
                              {from, From}
                             ]),
    {noreply, Data}.

handle_info({'EXIT', _Pid, _Reason}, Data) ->
    %% child exit?
    {noreply, Data};

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
	