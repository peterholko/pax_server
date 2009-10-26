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
-export([start/0, load_entities/0, setup_perception/0, add_event/4, update_perception/1, init/1, handle_call/3, handle_cast/2, 
        handle_info/2, terminate/2, code_change/3]).


%%
%% API Functions
%%

start() ->
    gen_server:start({global, game_pid}, game, [], []).

load_entities() ->
    gen_server:call({global, game_pid}, 'LOAD_ENTITIES').

setup_perception() ->
    gen_server:call({global, game_pid}, 'SETUP_PERCEPTION').

add_event(ObjectPid, EventType, EventData, EventTick) ->
    gen_server:cast(global:whereis_name(game_pid), {'ADD_EVENT', ObjectPid, EventType, EventData, EventTick}).

update_perception(PlayerId) ->
    gen_server:cast(global:whereis_name(game_pid), {'UPDATE_PERCEPTION', PlayerId}).

init([]) ->    
    Data = #game_info {update_perceptions = gb_sets:new() },
    {ok, Data}.

terminate(_Reason, _) ->
    ok.

handle_cast(stop, Data) ->
    {stop, normal, Data};

handle_cast({'ADD_PLAYER', PlayerId, ProcessId}, Data) ->
    
    %Toggle perception flag to send first perception
    NewUpdatePerceptions = gb_sets:add(PlayerId, Data#game_info.update_perceptions),
        
    ArmiesIdPid = gen_server:call(ProcessId, 'GET_ARMIES_ID_PID'),
    CitiesIdPid = gen_server:call(ProcessId, 'GET_CITIES_ID_PID'),
    
    NewEntities = ArmiesIdPid ++ CitiesIdPid,
    
    EntityList = Data#game_info.entities,
    NewEntityList = lists:append(NewEntities, EntityList),	
    
    NewPlayer = #player_process{player_id = PlayerId, process = ProcessId},
    PlayerList = Data#game_info.players,
    NewPlayerList = [NewPlayer|PlayerList],    
    
    NewData = Data#game_info {
                              players = NewPlayerList,
                              entities = NewEntityList,
                              update_perceptions = NewUpdatePerceptions                             
                             },
    {noreply, NewData};

handle_cast({'DELETE_PLAYER', PlayerId, ProcessId}, Data) ->
    io:fwrite("game - delete_player - ProcessId: ~w~n", [ProcessId]),
    %Toggle perception flag to send first perception
    NewUpdatePerceptions = gb_sets:delete(PlayerId, Data#game_info.update_perceptions),   
    
    ArmiesIdPid = gen_server:call(ProcessId, 'GET_ARMIES_ID_PID'),
    CitiesIdPid = gen_server:call(ProcessId, 'GET_CITIES_ID_PID'),
    
    Entities = ArmiesIdPid ++ CitiesIdPid,
    
    NewEntityList = Data#game_info.entities -- Entities,    
    NewPlayerList = lists:keydelete(PlayerId, 2, Data#game_info.players),
    NewData = Data#game_info {
                              players = NewPlayerList,
                              entities = NewEntityList,
                              update_perceptions = NewUpdatePerceptions
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

handle_cast({'ADD_BATTLE', BattlePid}, Data) ->
    BattleList = Data#game_info.battles,
    NewBattleList = [BattlePid | BattleList],
    NewData = Data#game_info { battles = NewBattleList},
    {noreply, NewData};

handle_cast({'DELETE_BATTLE', BattlePid}, Data) ->
    BattleList = Data#game_info.battles,
    NewBattleList = lists:delete(BattlePid, BattleList),
    NewData = Data#game_info { battles = NewBattleList},
    {noreply, NewData};

handle_cast('NEXT_TICK', Data) ->
    NextTick = Data#game_info.tick + 1,
    NewData = Data#game_info { tick = NextTick},
    {noreply, NewData};

handle_cast({'UPDATE_PERCEPTION', PlayerId}, Data) ->
    UpdatePerceptions = Data#game_info.update_perceptions,
    NewUpdatePerceptions = gb_sets:add(PlayerId, UpdatePerceptions),
    NewData = Data#game_info { update_perceptions = NewUpdatePerceptions},
    {noreply, NewData};

handle_cast('CLEAR_PERCEPTIONS', Data) ->
    NewData = Data#game_info { update_perceptions = gb_sets:empty()},
    {noreply, NewData}.

handle_call('LOAD_ENTITIES', _From, Data) ->
    %% Load armies, cities
    log4erl:info("Loading armies, cities and battles..."), 	
    ArmyIds = db:select_armies(),
    CityIds = db:select_cities(),
    BattleIds = db:select_battles(),
    
    log4erl:info("Starting armies, cities and battles processes..."),	
    lists:foreach(fun({ArmyId, PlayerId}) -> {ok, _Pid} = army:start(ArmyId, PlayerId) end, ArmyIds),
    lists:foreach(fun({CityId, PlayerId}) -> {ok, _Pid} = city:start(CityId, PlayerId) end, CityIds), 
    lists:foreach(fun(BattleId) -> {ok, _Pid} = battle:start(BattleId) end, BattleIds),
    log4erl:info("Armies, cities and battles processes started."),
    
    Armies = lists:foldr(fun({ArmyId,_}, Armies) -> [{ArmyId, global:whereis_name({army, ArmyId})} | Armies] end, [], ArmyIds),
    Cities = lists:foldr(fun({CityId,_}, Cities) -> [{CityId, global:whereis_name({city, CityId})} | Cities] end, [], CityIds),
    Battles = lists:foldr(fun(BattleId, Battles) -> [{BattleId, global:whereis_name({battle, BattleId})} | Battles] end, [], BattleIds),
    
    NewData = Data#game_info {armies = Armies, cities = Cities, battles = Battles},
    log4erl:info("All entities have been loaded."),	
    {reply, ok, NewData};

handle_call('SETUP_PERCEPTION', _From, Data) ->
    log4erl:info("Setup perception..."),
    entities_perception(Data#game_info.armies ++ Data#game_info.cities, Data#game_info.armies ++ Data#game_info.cities),		
    {reply, ok, Data};

handle_call({'IS_PLAYER_ONLINE', PlayerId}, _From, Data) ->	
    Result = lists:keymember(PlayerId, 2, Data#game_info.players),
    {reply, Result, Data};

handle_call('GET_UPDATE_PERCEPTION', _From, Data) ->
    {reply, gb_sets:to_list(Data#game_info.update_perceptions), Data};

handle_call('GET_PLAYERS', _From, Data) ->
    {reply, Data#game_info.players, Data};

handle_call('GET_ARMIES', _From, Data) ->
    {reply, Data#game_info.armies, Data};

handle_call('GET_CITIES', _From, Data) ->
    {reply, Data#game_info.cities, Data};

handle_call('GET_OBJECTS', _From, Data) ->
    
    %Improvements = gen_server:call(global:whereis_name(improve_pid), 'GET_IMPROVEMENTS'),
    Objects = Data#game_info.armies ++ Data#game_info.cities,% ++ Data#game_info.battles ++ Improvements,
    
    {reply, Objects , Data};

handle_call('GET_ENTITIES', _From, Data) ->
    {reply, Data#game_info.entities, Data};

handle_call('GET_BATTLES', _From, Data) ->
    {reply, Data#game_info.battles, Data};

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

entities_perception(Entities, EveryEntity) ->	
    log4erl:info("Updating all entities perception."),
    F = fun({EntityId, EntityPid}) ->
                {EntityX, EntityY, VisibleList, ObservedByList} = gen_server:call(EntityPid, 'GET_SUBSCRIPTION_DATA'),
                {ok, SubscriptionPid} = subscription:start(EntityId),									
                subscription:update_perception(SubscriptionPid, EntityId, EntityPid, EntityX, EntityY, EveryEntity, VisibleList, ObservedByList)
        end,
    
    lists:foreach(F, Entities),
    log4erl:info("All perception updated.").




