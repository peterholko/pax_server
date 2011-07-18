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
-export([start/0, queue/6, complete/1, info/2, empty/2, update_hp/3]).
-export([is_valid/3, check_type/1, find_available/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record(module_data, {}).
%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, improve_pid}, improvement, [], []).

queue(ImprovementId, X, Y, PlayerId, CityId, Type) ->
    gen_server:cast(global:whereis_name(improve_pid), {'QUEUE', ImprovementId, X, Y, PlayerId, CityId, Type}).

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

check_type(TypeId) ->
    case db:dirty_read(improvement_type, TypeId) of
        [_ImprovementType] ->
            Result = true;
        _ ->
            Result = false
    end,
    Result.

find_available(CityId, ItemTypeId) ->
    [ItemType] = db:dirty_read(item_type, ItemTypeId),
    {ImprovementTypeId, _ObjectType}  = ItemType#item_type.structure_req,
    Improvements = db:dirty_index_read(improvement, CityId, #improvement.city_id),

    check_available(Improvements, ImprovementTypeId, none).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    Data = #module_data {},
    {ok, Data}.

handle_cast({'QUEUE', ImprovementId, X, Y, PlayerId, CityId, Type}, Data) ->
    TileIndex = map:convert_coords(X, Y),
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
    
    io:fwrite("Improvement: ~w~n", [Improvement]), 

    db:dirty_write(Contract),
    db:dirty_write(ImprovementQueue),
    db:dirty_write(Improvement),
    
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
    
    EntityType = gen_server:call(EntityPid, {'GET_TYPE'}),
        
    case EntityType of
        ?OBJECT_ARMY ->
            add_observed_by(ImprovementId, EntityId, EntityPid);
        ?OBJECT_CITY ->
            add_observed_by(ImprovementId, EntityId, EntityPid)
    end, 

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

handle_call('GET_IMPROVEMENTS', _From, Data) ->
    
    [Improvements] = db:dirty_read(improvement),
    
    F = fun(Improvement, Rest) ->
                [{Improvement#improvement.id, self()} | Rest]
        end,
    
    ImprovementsIdPid = lists:foldr(F, [], Improvements),
    
    {reply, ImprovementsIdPid, Data};

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

handle_call({'GET_TYPE'}, _From, Data) ->
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
    
    F = fun({_EntityId, EntityPid}) ->                
                PlayerId = gen_server:call(EntityPid, {'GET_PLAYER_ID'}),                
                game:update_perception(PlayerId)
        end,  
    
    lists:foreach(F, ObservedByList).

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

check_available([], _ImprovementType, none) ->
    none;

check_available(_Improvements, _ImprovementType, {found, Improvement}) ->
    {found, Improvement};

check_available([Improvement | Rest], ImprovementType, Status) ->
    MatchType = Improvement#improvement.type =:= ImprovementType,
    ContractExists = contract:exists(Improvement#improvement.city_id,
                                     Improvement#improvement.id,
                                     ?CONTRACT_HARVEST),
    case MatchType and not ContractExists of
        true ->
            NewStatus = {found, Improvement};
        false ->
            NewStatus = Status
    end,

    check_available(Rest, ImprovementType, NewStatus).
