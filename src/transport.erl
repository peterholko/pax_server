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
%% --------------------------------------------------------------------
%% External exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(module_data, {}).
%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, transport_pid}, t, [], []).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    Data = #module_data {},
    {ok, Data}.

handle_cast({'ADD_CLAIM', CityId, X, Y}, Data) ->
    AddClaim = #add_claim {city_id = CityId, x = X, y = Y},
    packet:send(Data#data.socket, AddClaim),
    
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
            
            case gen_server:call(TargetPid, {'RECEIVE_UNIT', TargetId, Unit, Data#module_data.player_id}) of
                {receive_unit, success} ->
                    db:dirty_delete(unit, UnitId),
                    NewUnits = gb_sets:delete(UnitId, Units),
                    NewTransport = Transport#transport { units = NewUnits},
                    db:dirty_write(NewTransport),
                    TransferUnitInfo = {transfer_unit, success}; 
                Error ->
                    TransferUnitInfo = Error
            end;
        false ->
            TransferUnitInfo = {transfer_unit, error}
    end,

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

