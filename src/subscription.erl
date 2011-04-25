%%% -------------------------------------------------------------------
%%% Author  : Peter
%%% Description :
%%%
%%% Created : Oct 11, 2009
%%% -------------------------------------------------------------------
-module(subscription).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("common.hrl").
-include("game.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start/1, update_perception/8]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(module_data, {}).

%% ====================================================================
%% External functions
%% ====================================================================

start(EntityId) ->
    gen_server:start({global, {subscription, EntityId}}, subscription, [], []).

update_perception(Pid, EntityId, EntityPid, EntityX, EntityY, EveryObjectList, VisibleList, ObservedByList) ->
    spawn(fun() -> gen_server:call(Pid, {'UPDATE_PERCEPTION', EntityId, EntityPid, EntityX, EntityY, EveryObjectList, VisibleList, ObservedByList}), gen_server:cast(Pid, stop) end).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    Data = #module_data {},
    {ok, Data}.

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({'UPDATE_PERCEPTION', EntityId, EntityPid, EntityX, EntityY, EveryObjectList, VisibleList, ObservedByList}, _From, Data) ->
    
    subscription(EntityId, EntityPid, EntityX, EntityY, EveryObjectList, VisibleList, ObservedByList),
    
    {reply, ok, Data};

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

subscription(EntityId, EntityPid, EntityX, EntityY, EveryObjectList, VisibleList, ObservedByList) ->
    NewVisibleList = remove_visible_list(EntityId, EntityPid, EntityX, EntityY, VisibleList),
    NewObservedByList = remove_observed_by_list(EntityId, EntityPid, EntityX, EntityY, ObservedByList),
    
    ObjectList = lists:delete({EntityId, EntityPid}, EveryObjectList),	

    VisibleCandidateList = ObjectList -- NewVisibleList, 
    ObservedByCandidateList = ObjectList -- NewObservedByList,	

    add_visible_list(EntityId, EntityPid, EntityX, EntityY, VisibleCandidateList),
    add_observed_by_list(EntityId, EntityPid, EntityX, EntityY, ObservedByCandidateList).

remove_visible_list(SourceId, SourcePid, X, Y, Visible) ->	
    F = fun({EntityId, EntityPid}, VisibleList) ->				
                Distance = calc_distance(X, Y, EntityId, EntityPid),
                if
                    Distance >= ?GAME_VISION_RANGE ->
                        NewVisibleList = lists:delete({EntityId, EntityPid}, VisibleList),
                        gen_server:cast(SourcePid, {'REMOVE_VISIBLE', SourceId, EntityId, EntityPid}),										
                        gen_server:cast(EntityPid, {'REMOVE_OBSERVED_BY', EntityId, SourceId, SourcePid});
                    true ->
                        NewVisibleList = VisibleList
                end,
                NewVisibleList
        end,				
    
    lists:foldl(F, Visible, Visible).

remove_observed_by_list(SourceId, SourcePid, X, Y, ObservedBy) ->
    F = fun({EntityId, EntityPid}, ObservedByList) ->				
                Distance = calc_distance(X, Y, EntityId, EntityPid),
                
                if
                    Distance >= ?GAME_VISION_RANGE ->
                        NewObservedByList = lists:delete({EntityId, EntityPid}, ObservedByList),
                        gen_server:cast(SourcePid, {'REMOVE_OBSERVED_BY', SourceId, EntityId, EntityPid}),	
                        gen_server:cast(EntityPid, {'REMOVE_VISIBLE', EntityId, SourceId, SourcePid});										
                    true ->
                        NewObservedByList = ObservedByList
                end,
                NewObservedByList
        end,				
    
    lists:foldl(F, ObservedBy, ObservedBy).

add_visible_list(SourceId, SourcePid, X, Y, VisibleCandidateList) ->
    F2 = fun({EntityId, EntityPid}) ->
                 Distance = calc_distance(X, Y, EntityId, EntityPid),				 
                 
                 if
                     Distance < ?GAME_VISION_RANGE ->
                         gen_server:cast(SourcePid, {'ADD_VISIBLE', SourceId, EntityId, EntityPid}),
                         gen_server:cast(EntityPid, {'ADD_OBSERVED_BY', EntityId, SourceId, SourcePid});	
                     true ->
                         ok
                 end
         end,
    
    lists:foreach(F2, VisibleCandidateList).

add_observed_by_list(SourceId, SourcePid, X, Y, ObservedByCandidateList) ->
    F2 = fun({EntityId, EntityPid}) ->
                 Distance = calc_distance(X, Y, EntityId, EntityPid),				 
                 
                 if
                     Distance < ?GAME_VISION_RANGE ->
                         gen_server:cast(SourcePid, {'ADD_OBSERVED_BY', SourceId, EntityId, EntityPid}),
                         gen_server:cast(EntityPid, {'ADD_VISIBLE', EntityId, SourceId, SourcePid});	
                     true ->
                         ok
                 end
         end,
    
    lists:foreach(F2, ObservedByCandidateList).

calc_distance(X, Y, EntityId, EntityPid) ->
    EntityState = gen_server:call(EntityPid, {'GET_STATE', EntityId}),
    DiffX = X - EntityState#state.x,
    DiffY = Y - EntityState#state.y,
    DiffX * DiffX + DiffY * DiffY.
