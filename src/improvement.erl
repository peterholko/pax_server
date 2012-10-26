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
-export([start/0, queue/5, complete/1, info/2, empty/2, update_hp/3]).
-export([is_valid/3, check_type/1, is_available/1, match_req/2]).
-export([get_improvement_type/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record(module_data, {}).
%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, improve_pid}, improvement, [], []).

queue(X, Y, PlayerId, CityId, Type) ->
    gen_server:cast(global:whereis_name(improve_pid), {'QUEUE', X, Y, PlayerId, CityId, Type}).

complete(ImprovementId) ->
    gen_server:cast(global:whereis_name(improve_pid), {'COMPLETE', ImprovementId}).

update_hp(ImprovementId, TotalHp, CompletionRatio) ->
    gen_server:cast(global:whereis_name(improve_pid), {'UPDATE_HP', ImprovementId, TotalHp, CompletionRatio}).

info(ImprovementId, PlayerId) ->
    gen_server:call(global:whereis_name(improve_pid), {'GET_INFO', ImprovementId, PlayerId}).

empty(X, Y) ->
    gen_server:call(global:whereis_name(improve_pid), {'CHECK_EMPTY', X, Y}).

is_valid(_CheckClaim = true, _CheckEmpty = true, _CheckType = true) ->
    true;
is_valid(_CheckClaim = false, _CheckEmpty, _CheckType) ->
    {false, not_claimed};
is_valid(_CheckClaim, _CheckEmpty = false, _CheckType) ->
    {false, not_empty};
is_valid(_CheckClaim, _CheckEmpty, _CheckType = false) ->
    {false, invalid_type}.

match_req(ReqName, ReqLevel) ->
    case db:dirty_match_object({improvement_type,'_',ReqName,ReqLevel,'_','_','_','_','_','_','_','_'}) of
        [ImprovementType] ->
            Result = {true, ImprovementType};
        _ ->
            Result = false
    end,
    Result.

get_improvement_type(TypeId) ->
    case db:dirty_read(improvement_type, TypeId) of
        [ImprovementType] ->
            Result = {true, ImprovementType};
        _ ->
            Result = false
    end,
    Result.

check_type(TypeId) ->
    case db:dirty_read(improvement_type, TypeId) of
        [_ImprovementType] ->
            Result = true;
        _ ->
            Result = false
    end,
    Result.

is_available(ImprovementId) ->
    case db:dirty_read(improvement, ImprovementId) of
        [Improvement] ->
            ContractExists = contract:exists(Improvement#improvement.city_id,
                                             Improvement#improvement.id,
                                             ?CONTRACT_HARVEST),
            case ContractExists of
                true ->
                    IsAvailable = false;
                false ->
                    case Improvement#improvement.state == ?STATE_COMPLETED of
                        true -> IsAvailable = {true, Improvement};
                        false -> IsAvailable = false
                    end
            end;
        _ ->
            IsAvailable = false
    end,
    IsAvailable.        

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    Data = #module_data {},
    {ok, Data}.

handle_cast({'QUEUE', X, Y, PlayerId, CityId, Type}, Data) ->
    TileIndex = map:convert_coords(X, Y),

    ImprovementPid = self(),
    ImprovementId = counter:increment(entity),
    Improvement = #improvement{id = ImprovementId,
                               tile_index = TileIndex,
                               player_id = PlayerId, 
                               city_id = CityId,
                               type = Type,                                
                               state = ?STATE_CONSTRUCTING,
                               hp = 0,
                               observed_by = []},

    CurrentTime = util:get_time_seconds(),
    ContractId = counter:increment(contract),
    TargetRef = {ImprovementId, ?OBJECT_IMPROVEMENT},

    Contract = #contract {id = ContractId,
                          city_id = CityId,
                          type = ?CONTRACT_IMPROVEMENT,
                          target_ref = TargetRef,
                          object_type = Type,
                          production = 0,
                          created_time = CurrentTime,
                          last_update = CurrentTime},

    ImprovementQueue = #improvement_queue {contract_id = ContractId,
                                           improvement_id = ImprovementId,
                                           improvement_type = Type},
    
    ?INFO("Starting subscription module"),
    %Subscription update
    {ok, SubPid} = subscription:start(ImprovementId),

    ?INFO("Updating subscription perception"),
    subscription:update_perception(SubPid, ImprovementId, ImprovementPid, X, Y, [], []),

    %Toggle player's perception has been updated.
    ?INFO("Updating game perception"),
    game:update_perception(PlayerId),

    db:dirty_write(Contract),
    db:dirty_write(ImprovementQueue),
    db:dirty_write(Improvement),
    
    game:add_improvement(ImprovementId, self()),

    {noreply, Data};

handle_cast({'COMPLETE', ImprovementId}, Data) ->
    [Improvement] = db:dirty_read(improvement, ImprovementId),
    [ImprovementType] = db:dirty_read(improvement_type, Improvement#improvement.type),

    TotalHp = ImprovementType#improvement_type.total_hp,
    NewImprovement = Improvement#improvement {hp = TotalHp,
                                              state = ?STATE_COMPLETED},

    db:dirty_write(NewImprovement),
    update_observers(NewImprovement),

    {noreply, Data};

handle_cast({'UPDATE_HP', ImprovementId, TotalHp, CompletionRatio}, Data) ->
    [Improvement] = db:dirty_read(improvement, ImprovementId),
    NewHp = util:round3(TotalHp * CompletionRatio),
    NewImprovement = Improvement#improvement {hp = NewHp},

    db:dirty_write(NewImprovement),
    {noreply, Data};

handle_cast({'ADD_VISIBLE', _ImprovementId, _EntityId, _EntityPid}, Data) ->    
    %Do nothing atm 
    {noreply, Data};

handle_cast({'REMOVE_VISIBLE', _ImprovementId, _EntityId, _EntityPid}, Data) ->
    %Do nothing atm	
    {noreply, Data};

handle_cast({'ADD_OBSERVED_BY', ImprovementId, EntityId, EntityPid}, Data) ->    
    add_observed_by(ImprovementId, EntityId, EntityPid),

    {noreply, Data};

handle_cast({'REMOVE_OBSERVED_BY', ImprovementId, EntityId, EntityPid}, Data) ->    
    remove_observed_by(ImprovementId, EntityId, EntityPid),
    
    {noreply, Data};

handle_cast({'PROCESS_EVENT', _EventTick, _ImprovementId, _EventType}, Data) ->
    
    %case EventType of
        %?EVENT_IMPROVEMENT_COMPLETED ->  
   %end,      
    
    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({'CHECK_EMPTY', X, Y}, _From, Data) ->
    TileIndex = map:convert_coords(X, Y),

    case db:dirty_index_read(improvement, TileIndex, #improvement.tile_index) of
        [_Improvement] ->
            Empty = false;
        _ ->
            Empty = true
    end,    

    {reply, Empty, Data};

handle_call({'GET_STATE', ImprovementId}, _From, Data) ->
    case db:dirty_read(improvement, ImprovementId) of
        [Improvement] ->
            {TileX, TileY} = map:convert_coords(Improvement#improvement.tile_index),			
            State = #state {id = ImprovementId,
                            player_id = Improvement#improvement.player_id,
                            type = ?OBJECT_IMPROVEMENT,
                            subtype = Improvement#improvement.type,
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

handle_call({'GET_TYPE', _ImprovementId}, _From, Data) ->
    {reply, ?OBJECT_IMPROVEMENT, Data};

handle_call({'GET_INFO', ImprovementId, _PlayerId}, _From, Data) ->   
    case db:dirty_read(improvement, ImprovementId) of
        [Improvement] ->
            Info = {detailed, Improvement#improvement.type};
        _ ->
            Info = none
    end,
    {reply, Info, Data};

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

update_observers(Improvement) ->
    ObservedByList = Improvement#improvement.observed_by, 
    Self = self(),    

    F = fun({EntityId, EntityPid}) ->  
            if 
                EntityPid =:= Self ->
                    PlayerId = get_player_id(EntityId);
                true ->
                    PlayerId = gen_server:call(EntityPid, {'GET_PLAYER_ID'})
            end,
            game:update_perception(PlayerId)
        end,  
    
    lists:foreach(F, ObservedByList).

get_player_id(Id) ->
    [Improvement] = db:dirty_read(improvement, Id),
    Improvement#improvement.player_id.

add_observed_by(ImprovementId, EntityId, EntityPid) ->
    [Improvement] = db:dirty_read(improvement, ImprovementId),
    NewObservedByList = [{EntityId, EntityPid} | Improvement#improvement.observed_by],
    NewImprovement = Improvement#improvement { observed_by = NewObservedByList},
    db:dirty_write(NewImprovement).

remove_observed_by(ImprovementId, EntityId, EntityPid) ->
    [Improvement] = db:dirty_read(improvement, ImprovementId),
    NewObservedByList = lists:delete({EntityId, EntityPid}, Improvement#improvement.observed_by),
    NewImprovement = Improvement#improvement { observed_by = NewObservedByList},
    db:dirty_write(NewImprovement).                                  

