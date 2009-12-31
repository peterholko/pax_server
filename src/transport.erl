%% -------------------------------------------------------------------
%% Author  : Peter
%%% Description :
%%%
%%% Created : Nov 26, 2009
%%% -------------------------------------------------------------------
-module(transport).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("schema.hrl").
-include("common.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(module_data, {}).
%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, transport_pid}, transport, [], []).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    Data = #module_data {},
    {ok, Data}.

handle_cast('TEST', Data) ->   
    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({'TRANSFER_UNIT', SourceId, UnitId, TargetId, TargetAtom}, _From, Data) ->   
    [Transport] = db:dirty_read(transport, SourceId),        
    Units = Transport#transport.units,
    
    case gb_sets:is_member(UnitId, Units) of
        true ->
            [Unit] = db:dirty_read(unit, UnitId),
            TargetPid = object:get_pid(TargetAtom, TargetId),
            
            case gen_server:call(TargetPid, {'RECEIVE_UNIT', TargetId, Unit, Transport#transport.player_id}) of
                {receive_unit, success} ->
                    NewUnits = gb_sets:delete(UnitId, Units),
                    NewTransport = Transport#transport { units = NewUnits}
,
                    db:dirty_write(NewTransport),

                    TransferUnitInfo = {transfer_unit, success}; 
                Error ->
                    TransferUnitInfo = Error
            end;
        false ->
            TransferUnitInfo = {transfer_unit, error}
    end,

    {reply, TransferUnitInfo, Data};

handle_call({'RECEIVE_UNIT', TargetId, Unit, PlayerId}, _From, Data) ->  
    [Transport] = db:dirty_read(transport, TargetId),
    Units = Transport#transport.units,
    
    if
        PlayerId =:= Transport#transport.player_id ->
            %Check to see if unit already exists
            UnitResult = gb_sets:is_member(Unit#unit.id, Units),            
            
            if
                UnitResult =:= false ->                    
                    NewUnit = Unit#unit {entity_id = {?OBJECT_TRANSPORT, TargetId},
                                         entity_type = ?OBJECT_TRANSPORT},
                    
                    NewUnits = gb_sets:add(NewUnit#unit.id, Units),            
                    NewTransport = Transport#transport { units = NewUnits },

                    db:dirty_write(NewUnit),       
                    db:dirty_write(NewTransport),                
                    
                    ReceiveUnitInfo = {receive_unit, success};                
                true ->
                    ReceiveUnitInfo = {receive_unit, error}
            end;               
        true ->
            ReceiveUnitInfo = {receive_unit, error}
    end,
    
    {reply, ReceiveUnitInfo, Data};    

handle_call({'GET_INFO', TransportId}, _From, Data) ->   
    Units = db:dirty_index_read(unit, {?OBJECT_TRANSPORT, TransportId}, #unit.entity_id), 
    UnitsInfoTuple = unit:units_tuple(Units),
    TransportInfo = {detailed, TransportId, UnitsInfoTuple},
    
    {reply, TransportInfo , Data};

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

