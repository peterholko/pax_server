%% -------------------------------------------------------------------
%% Author  : Peter
%%% Description :
%%%
%%% Created : Feb, 2012
%%% -------------------------------------------------------------------
-module(army_manager).
-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("schema.hrl").
-include("packet.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([create/4]).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, army_manager_pid}, army_manager, [], []).

create(PlayerId, X, Y, ArmyName) ->
    gen_server:cast({global, army_manager_pid}, {'CREATE', PlayerId, X, Y, ArmyName}).
%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    {ok, []}.

handle_cast({'CREATE', PlayerId, X, Y, ArmyName}, Data) ->  
    ArmyId = counter:increment(entity) + 5000,
    Army = #army {id = ArmyId,
                  player_id = PlayerId,
                  name = ArmyName,
                  x = X,
                  y = Y,
                  state = ?STATE_EMPTY},

    db:dirty_write(Army),

    %Add army to kingdom
    kingdom:add_army(PlayerId, ArmyId),

    %Add entity
    entity:add_entity(ArmyId, PlayerId, ?OBJECT_ARMY),

    %Start army process
    {ok, ArmyPid} = army:start(ArmyId, PlayerId),

    ?INFO("Starting subscription module"),
    %Subscription update
    {ok, SubPid} = subscription:start(ArmyId),

    ?INFO("Updating subscription perception"),
    subscription:update_perception(SubPid, ArmyId, ArmyPid, X, Y, [], []),

    %Add to game
    game:add_army(ArmyId, ArmyPid),

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
