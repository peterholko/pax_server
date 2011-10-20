%% -------------------------------------------------------------------
%% Author  : Peter
%%% Description :
%%%
%%% Created : Sept, 2011
%%% -------------------------------------------------------------------
-module(chat).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("schema.hrl").
-include("common.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(data, {members = []}).
-record(member, {player_id,
                 player_pid,
                 player_name}).
%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, chat_pid}, chat, [], []).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    Data = #data {},
    {ok, Data}.

handle_cast('TEST', Data) ->   
    {noreply, Data};

handle_cast({'ADD', PlayerId, PlayerName, Socket}, Data) ->
    Member = #member {player_id = PlayerId,
                      player_name = PlayerName},

    NewMembers = [Member | Members],
    NewData = Data#members { members = NewMembers},

    {noreply, NewData};

handle_cast({'SEND_MSG', 

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({'GET_ARMIES', PlayerId}, _From, Data) ->
    [Kingdom] = db:dirty_index_read(kingdom, PlayerId, #kingdom.player_id), 
    {reply, Kingdom#kingdom.armies, Data};

handle_call({'GET_ARMIES_ID_PID', PlayerId}, _From, Data) ->
    [Kingdom] = db:dirty_index_read(kingdom, PlayerId, #kingdom.player_id), 
    Armies = Kingdom#kingdom.armies,
    
    F = fun(ArmyId, Rest) -> [{ArmyId, global:whereis_name({army, ArmyId})} | Rest] end,
    ArmiesIdPid = lists:foldl(F, [], Armies),	
    {reply, ArmiesIdPid, Data};

handle_call({'GET_CITIES_ID_PID', PlayerId}, _From, Data) ->
    [Kingdom] = db:dirty_index_read(kingdom, PlayerId, #kingdom.player_id), 
    Cities = Kingdom#kingdom.cities,
    
    F = fun(CityId, Rest) -> [{CityId, global:whereis_name({city, CityId})} | Rest] end,
    CitiesIdPid = lists:foldl(F, [], Cities),	
    {reply, CitiesIdPid, Data};

handle_call({'GET_GOLD', PlayerId}, _From, Data) ->
    [Kingdom] = db:dirty_index_read(kingdom, PlayerId, #kingdom.player_id), 
    {reply, Kingdom#kingdom.gold, Data};

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

terminate(_Reason, _) ->
    ok.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

