%% -------------------------------------------------------------------
%% Author  : Peter
%%% Description :
%%%
%%% Created : June 23, 2010
%%% -------------------------------------------------------------------
-module(map_object).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("schema.hrl").
-include("common.hrl").
-include("game.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([create/4]).

-record(data, {}).
%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, map_object_pid}, map_object, [], []).

create(ObjectId, ObjectType, X, Y) ->
    gen_server:cast({global, map_object_pid}, {'CREATE', ObjectId, ObjectType, X, Y}).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
   Data = #data {},
    {ok, Data}.

handle_cast({'CREATE', ObjectId, ObjectType, X, Y}, Data) ->
    MapObject = #map_object {id = ObjectId,
                             type = ObjectType, 
                             x = X,
                             y = Y,
                             observed_by = []},

    db:dirty_write(MapObject),

    ?INFO("Starting subscription module"),
    %Subscription update
    {ok, SubPid} = subscription:start(ObjectId),
    
    ?INFO("Updating subscription perception"),
    subscription:update_perception(SubPid, ObjectId, self(), X, Y, [], []),
    
    game:add_map_object(ObjectId, self()),
    {noreply, Data};

handle_cast({'ADD_VISIBLE', _MapObjectId, _EntityId, _EntityPid}, Data) ->
    %Do nothing atm 
    {noreply, Data};

handle_cast({'REMOVE_VISIBLE', _MapObjectId, _EntityId, _EntityPid}, Data) ->
    %Do nothing atm 
    {noreply, Data};

handle_cast({'ADD_OBSERVED_BY', MapObjectId, EntityId, EntityPid}, Data) ->
    add_observed_by(MapObjectId, EntityId, EntityPid),
    {noreply, Data};

handle_cast({'REMOVE_OBSERVED_BY', MabOjectId, EntityId, EntityPid}, Data) ->
    remove_observed_by(MabOjectId, EntityId, EntityPid),
    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({'GET_STATE', MapObjectId}, _From, Data) ->
    case db:dirty_read(map_object, MapObjectId) of
        [MapObject] ->
            State = #state { id = MapObjectId,
                             player_id = ?PLAYER_NONE,
                             type = MapObject#map_object.type,
                             subtype = ?OBJECT_BASIC,
                             state = ?STATE_NONE,
                             x = MapObject#map_object.x,
                             y = MapObject#map_object.y};
        _ ->
            State = #state { id = -1,
                             player_id = -1,
                             type = -1,
                             subtype = -1,
                             state = -1,
                             x = -1,
                             y = -1}
    end,

    {reply, State, Data};

handle_call({'GET_TYPE', MapObjectId}, _From, Data) ->
    case db:dirty_read(map_object, MapObjectId) of
        [MapObject] ->
            Type = MapObject#map_object.type;
        _ ->
            Type = ?OBJECT_NONE
    end,
    {reply, Type, Data};

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
add_observed_by(MapObjectId, EntityId, EntityPid) ->
    [MapObject] = db:dirty_read(map_object, MapObjectId),
    NewObservedByList = [{EntityId, EntityPid} | MapObject#map_object.observed_by],
    NewMapObject = MapObject#map_object { observed_by = NewObservedByList},
    db:dirty_write(NewMapObject).

remove_observed_by(MabOjectId, EntityId, EntityPid) ->
    [MabOject] = db:dirty_read(improvement, MabOjectId),
    NewObservedByList = lists:delete({EntityId, EntityPid}, MabOject#improvement.observed_by),
    NewMabOject = MabOject#improvement { observed_by = NewObservedByList},
    db:dirty_write(NewMabOject).
