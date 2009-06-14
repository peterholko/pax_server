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
    io:fwrite("Loading armies, cities and battles...~n"), 
    ArmyIds = db:select_armies(),
	CityIds = db:select_cities(),
    BattleIds = db:select_battles(),
        
    io:fwrite("Starting armies, cities and battles processes...~n"), 
    lists:foreach(fun({ArmyId, PlayerId}) -> army:start(ArmyId, PlayerId) end, ArmyIds),
    lists:foreach(fun({CityId, PlayerId}) -> city:start(CityId, PlayerId) end, CityIds), 
    lists:foreach(fun(BattleId) -> battle:start(BattleId) end, BattleIds),
    
    ArmyPids = lists:foldr(fun({X,_}, Pids) -> [global:whereis_name({army, X}) | Pids] end, [], ArmyIds),
	CityPids = lists:foldr(fun({Y,_}, Pids) -> [global:whereis_name({city, Y}) | Pids] end, [], CityIds),
	BattlePids = lists:foldr(fun(BattleId, Pids) -> [global:whereis_name({battle, BattleId}) | Pids] end, [], BattleIds),
    
	Data = #game_info {armies = ArmyPids, cities = CityPids, battles = BattlePids},
	{ok, Data}.

terminate(_Reason, _) ->
    ok.

handle_cast(stop, Data) ->
    {stop, normal, Data};

handle_cast({'ADD_PLAYER', PlayerId, ProcessId, NewEntities}, Data) ->
	NewPlayer = #player_process{player_id = PlayerId, process = ProcessId},

   	EntityList = Data#game_info.entities,
	NewEntityList = lists:append(NewEntities, EntityList),	

    PlayerList = Data#game_info.players,
	NewPlayerList = [NewPlayer|PlayerList],    
	
	NewData = Data#game_info {
            		players = NewPlayerList,
					entities = NewEntityList
           			},
    {noreply, NewData};

handle_cast({'DELETE_PLAYER', PlayerId, ProcessId}, Data) ->
	io:fwrite("game - delete_player - ProcessId: ~w~n", [ProcessId]),
    ArmiesPid = gen_server:call(ProcessId, 'GET_ARMIES_PID'),
    NewEntityList = Data#game_info.entities -- ArmiesPid,    
	NewPlayerList = lists:keydelete(PlayerId, 2, Data#game_info.players),
	NewData = Data#game_info {
            		players = NewPlayerList,
                    entities = NewEntityList
           			},
    io:fwrite("game - delete_player ~n"),
	{noreply, NewData};

handle_cast({'ADD_EVENT', Pid, Type, EventData, EventTick}, Data) ->
    io:fwrite("game - add_event - EventTick: ~w~n", [EventTick]),
    EventId = counter:increment(event),
    CurrentTick = Data#game_info.tick,
    Events = Data#game_info.events,
    NewEvents = [{EventId, Pid, Type, EventData, CurrentTick + EventTick} | Events],
    NewData = Data#game_info { events = NewEvents},  
    
    {noreply, NewData};

handle_cast({'DELETE_EVENT', EventId}, Data) ->
    EventList = Data#game_info.events,
    NewEventList = lists:keydelete(EventId, 1, EventList),
    NewData = Data#game_info {events = NewEventList},
    {noreply, NewData};

handle_cast({'CLEAR_EVENTS', Pid}, Data) ->
    EventList = Data#game_info.events,
    NewEventList = lists:keydelete(Pid, 2, EventList),
    NewData = Data#game_info {events = NewEventList},
    {noreply, NewData};

handle_cast('NEXT_TICK', Data) ->
    NextTick = Data#game_info.tick + 1,
    NewData = Data#game_info { tick = NextTick},
    {noreply, NewData}.

handle_call({'IS_PLAYER_ONLINE', PlayerId}, _From, Data) ->	
	Result = lists:keymember(PlayerId, 2, Data#game_info.players),
	{reply, Result, Data};

handle_call('GET_PLAYERS', _From, Data) ->
	{reply, Data#game_info.players, Data};

handle_call('GET_ARMIES', _From, Data) ->
	{reply, Data#game_info.armies, Data};

handle_call('GET_CITIES', _From, Data) ->
	{reply, Data#game_info.cities, Data};

handle_call('GET_OBJECTS', _From, Data) ->
    {reply, Data#game_info.armies ++ Data#game_info.cities, Data};

handle_call('GET_ENTITIES', _From, Data) ->
	{reply, Data#game_info.entities, Data};

handle_call('GET_EVENTS', _From, Data) ->
	{reply, Data#game_info.events, Data};    

handle_call('GET_TICK', _From, Data) ->
    {reply, Data#game_info.tick, Data};

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
    
    
    


