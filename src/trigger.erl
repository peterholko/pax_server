%% -------------------------------------------------------------------
%% Author  : Peter
%%% Description :
%%%
%%% Created : April, 2013
%%% -------------------------------------------------------------------
-module(trigger).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("schema.hrl").
-include("packet.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([add/2]).
%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, trigger_pid}, trigger, [], []).

add(TriggerType, TriggerData) ->
    gen_server:cast({global, trigger_pid}, {'ADD', TriggerType, TriggerData}).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    Data = [],
    {ok, Data}.

handle_cast({'ADD', TriggerType, TriggerData}, Data) ->
    ?INFO("Added trigger type: ", TriggerType),
    case TriggerType of
        ?TRIGGER_MOVE ->
            move(TriggerData);
        _ ->
            ?INFO("Invalid trigger type: ", TriggerType)
    end,

    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

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

move(TriggerData) ->
    {ArmyId, X, Y} = TriggerData,

    TileType = map:get_tile_type(X, Y),
    AmbushRate = ambush_rate(TileType),
    Random = random:uniform(),

    ?INFO("AmbushRate: ", AmbushRate),
    ?INFO("Random: ", Random),

    case AmbushRate =< Random of
        true -> spawn_ambush(ArmyId, X, Y);
        false -> do_nothing
    end.

ambush_rate(TileType) ->
    case TileType of
        ?TILE_MOUNTAIN -> ?TILE_MOUNTAIN_AMBUSH;
        ?TILE_FOREST -> ?TILE_FOREST_AMBUSH;
        ?TILE_PLAINS -> ?TILE_PLAINS_AMBUSH;
        ?TILE_SWAMP -> ?TILE_SWAMP_AMBUSH
    end.

spawn_ambush(ArmyId, X, Y) ->
    %Spawn NPC army
    NPCId = -1,
    NPCArmyId = army_manager:create(NPCId, X, Y, <<"Ambushers">>),
    unit:create(NPCArmyId, 1, 100, []), 

    %FIXME Remove sleep
    timer:sleep(1000),

    %Create battle
    BattleId = counter:increment(battle) + 1000000,
    battle:create(BattleId, X, Y),
    battle:setup(BattleId, NPCArmyId, ArmyId),
    
    %Set army states combat
    army:combat(NPCArmyId, BattleId),
    army:combat(ArmyId, BattleId).
