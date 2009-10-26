%%% -------------------------------------------------------------------
%%% Author  : Peter
%%% Description : Improvements Process
%%%
%%% Created : Oct 3, 2009
%%% -------------------------------------------------------------------
-module(improvement).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("common.hrl").
-include("game.hrl").
-include("schema.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start/0,create/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(module_data, {}).
%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, improve_pid}, improvement, [], []).

create(TileX, TileY, PlayerId, Type) ->
    gen_server:cast(global:whereis_name(improve_pid), {'BUILD_IMPROVEMENT', TileX, TileY, PlayerId, Type}).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    Data = #module_data {},
    {ok, Data}.

handle_cast({'BUILD_IMPROVEMENT', TileX, TileY, PlayerId, Type}, Data) ->
    
    TileIndex = map:convert_coords(TileX, TileY),
    Improvement = #improvement{id = counter:increment(improvement),
                               tile_index = TileIndex,
                               player_id = PlayerId, 
                               type = Type,                                
                               state = ?STATE_CONSTRUCTING,
                               observed_by},
    
    db:dirty_write(Improvement),
    
    %% Add completion event 
    game:add_event(self(), ?EVENT_IMPROVEMENT_COMPLETED, Improvement#improvement.id, 10),
    
    {noreply, Data};

handle_cast({'ADD_VISIBLE', ImprovementId, _ImprovementPid}, Data) ->    
    %[Improvement] = db:dirty_read(improvement, ImprovementId),
    {noreply, Data};

handle_cast({'REMOVE_VISIBLE', _ImprovementId, _ImprovementPid}, Data) ->
    %Do nothing atm	
    {noreply, Data};

handle_cast({'PROCESS_EVENT', ImprovementId, EventType}, Data) ->
    
    case EventType of
        ?EVENT_IMPROVEMENT_COMPLETED ->           
            state_completed(ImprovementId)            
    end,      
    
    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call('GET_IMPROVEMENTS', _From, Data) ->
    
    [Improvements] = db:dirty_read(improvement),
    
    F = fun(Improvement, Rest) ->
                [{Improvement#improvement.id, self()} | Rest]
        end,
    
    ImprovementsIdPid = lists:foldr(F, [], Improvements),
    
    {reply, ImprovementsIdPid, Data};

handle_call({'GET_STATE', ImprovementId}, _From, Data) ->
    
    [Improvement] = db:dirty_read(improvement, ImprovementId),
    
    case db:dirty_read(improvement, ImprovementId) of
        [Improvement] ->
            {TileX, TileY} = map:convert_coords(Improvement#improvement.tile_index),			
            State = #state {id = ImprovementId,
                            player_id = Improvement#improvement.player_id,
                            type = ?OBJECT_IMPROVEMENT,
                            state = Improvement#improvement.state,
                            x = TileX,
                            y = TileY};			
        _ ->
            State = #state {id = ImprovementId,
                            player_id = none,
                            type = ?OBJECT_IMPROVEMENT,
                            state = ?STATE_DEAD,
                            x = 10000000,
                            y = 10000000}
    end,
    
    {reply, State, Data};


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

%% ====================================================================
%% Internal functions
%% ====================================================================

state_completed(ImprovementId) ->
    log4erl:info("Improvement completed"),
    F = fun() -> 
                [Improvement] = mnesia:read(improvement, ImprovementId),
                NewImprovement = Improvement#improvement{state = ?STATE_COMPLETED},
                mnesia:write(NewImprovement)
        end,
    {atomic, Value} = mnesia:transaction(F),
    Value.

%% check_queue(Improvement) ->
%%     [ImprovementQueue] = db:dirty_read(improvement_queue, Improvement#improvement.id),
%%     
%%     CurrentTime = util:get_time_seconds(),
%%     
%%     if
%%         ImprovementQueue#improvement_queue.end_time =< CurrentTime ->
%%             log4erl:info("Improvement Complete"),
%%             
%%             NewImprovement = Improvement#improvement {state = ?STATE_COMPLETE},			
%%             db:dirty_delete(improvement_queue, Improvement#improvement.id),
%%             db:dirty_write(NewImprovement);
%%         true ->
%%             ok
%%     end.

