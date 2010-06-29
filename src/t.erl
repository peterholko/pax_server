%% -------------------------------------------------------------------
%% Author  : Peter
%%% Description :
%%%
%%% Created : Oct 11, 2009
%%% -------------------------------------------------------------------
-module(t).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("packet.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start/1, login/1]).
-export([build_farm/0, add_claim/0, assign_task/0, transfer/0, transfer2/0, 
         battle/0, target/0, info_army/1, info_city/1, move/3, 
         queue_unit/0, queue_building/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(data, {socket}).

%% ====================================================================
%% External functions
%% ====================================================================

start(Socket) ->
    gen_server:start({global, test_sender}, t, [Socket], []).

login(Account) ->
    gen_server:cast(global:whereis_name(test_sender), {'LOGIN', Account}).

build_farm() ->
    gen_server:cast(global:whereis_name(test_sender), {'BUILD_IMPROVEMENT', 11, 5, 5, ?IMPROVEMENT_FARM}).

queue_unit() ->
    gen_server:cast(global:whereis_name(test_sender), {'QUEUE_UNIT', 11, 1, 100, 0}).

queue_building() ->
    gen_server:cast(global:whereis_name(test_sender), {'QUEUE_BUILDING', 11, 1}).

add_claim() ->
    gen_server:cast(global:whereis_name(test_sender), {'ADD_CLAIM', 11, 5, 5}).

assign_task() ->
    gen_server:cast(global:whereis_name(test_sender), {'ASSIGN_TASK', 11, 0, 33, 1, ?TASK_CONSTRUCTION}).

transfer() -> 
    gen_server:cast(global:whereis_name(test_sender), {'TRANSFER_UNIT', 1, 1, 1, 2, 1}),
    gen_server:cast(global:whereis_name(test_sender), {'TRANSFER_UNIT', 7, 2, 1, 1, 1}).

transfer2() ->
    gen_server:cast(global:whereis_name(test_sender), {'TRANSFER_UNIT', 1, 1, 1, 1, 6}).

battle() ->
    gen_server:cast(global:whereis_name(test_sender), {'ATTACK', 1, 3}).

target() ->
    gen_server:cast(global:whereis_name(test_sender), {'TARGET', 1, 1, 1, 3, 9}).

info_army(ArmyId) ->
    gen_server:cast(global:whereis_name(test_sender), {'INFO_ARMY', ArmyId}).

info_city(CityId) ->
    gen_server:cast(global:whereis_name(test_sender), {'INFO_CITY', CityId}).

move(ArmyId, X, Y) ->
    gen_server:cast(global:whereis_name(test_sender), {'MOVE', ArmyId, X, Y}).


%% ====================================================================
%% Server functions
%% ====================================================================

init([Socket]) ->
    Data = #data {socket = Socket},
    {ok, Data}.

handle_cast({'ASSIGN_TASK', CityId, PopulationId, Amount, TaskId, TaskType}, Data) ->
    AssignTask = #assign_task { city_id = CityId, 
                                population_id = PopulationId, 
                                amount = Amount, 
                                task_id = TaskId,
                                task_type = TaskType},
    packet:send(Data#data.socket, AssignTask),
    
    {noreply, Data};

handle_cast({'ADD_CLAIM', CityId, X, Y}, Data) ->
    AddClaim = #add_claim {city_id = CityId, x = X, y = Y},
    packet:send(Data#data.socket, AddClaim),
    
    {noreply, Data};

handle_cast({'BUILD_IMPROVEMENT', CityId, X, Y, ImprovementType}, Data) ->
    BuildImprovement = #build_improvement {city_id = CityId, x = X, y = Y, improvement_type = ImprovementType},
    packet:send(Data#data.socket, BuildImprovement),

    {noreply, Data};

handle_cast({'QUEUE_UNIT', CityId, UnitType, UnitSize, Caste}, Data) ->
    CityQueueUnit = #city_queue_unit { id = CityId,
                                       unit_type = UnitType,
                                       unit_size = UnitSize,
                                       caste = Caste},
    packet:send(Data#data.socket, CityQueueUnit),
    {noreply, Data};

handle_cast({'QUEUE_BUILDING', CityId, BuildingType}, Data) ->
    CityQueueBuilding = #city_queue_building { id = CityId,
                                               building_type = BuildingType},
    packet:send(Data#data.socket, CityQueueBuilding),
    {noreply, Data};

handle_cast({'LOGIN', Account}, Data) ->
    AccountBin = list_to_binary(Account),
    gen_tcp:send(Data#data.socket, <<?CMD_LOGIN, 5:16, AccountBin/binary, 6:16, "123123">>),
    {noreply, Data};

handle_cast({'TRANSFER_UNIT', UnitId, SourceId, SourceType, TargetId, TargetType}, Data) ->
    TransferUnit = #transfer_unit { unit_id = UnitId, 
                                    source_id = SourceId,
                                    source_type = SourceType, 
                                    target_id = TargetId,
                                    target_type = TargetType},
    packet:send(Data#data.socket, TransferUnit),
    {noreply, Data};

handle_cast({'INFO_ARMY', ArmyId}, Data) ->
    RequestInfo = #request_info{type = ?OBJECT_ARMY, id = ArmyId},
    packet:send(Data#data.socket, RequestInfo),
    {noreply, Data};  

handle_cast({'INFO_CITY', CityId}, Data) ->
    RequestInfo = #request_info{type = ?OBJECT_CITY, id = CityId},
    packet:send(Data#data.socket, RequestInfo),
    {noreply, Data};   
 
handle_cast({'MOVE', ArmyId, X, Y}, Data) ->
    Move = #move{id = ArmyId, x = X, y = Y},
    packet:send(Data#data.socket, Move),
    {noreply, Data};    

handle_cast({'ATTACK', SourceId, TargetId}, Data) ->
    Attack = #attack{id = SourceId, target_id = TargetId},
    packet:send(Data#data.socket, Attack),
    {noreply, Data};

handle_cast({'TARGET', BattleId, SourceArmy, SourceUnit, TargetArmy, TargetUnit}, Data) ->
    Target = #battle_target{battle_id = BattleId, 
                            source_army_id = SourceArmy, 
                            source_unit_id = SourceUnit,
                            target_army_id = TargetArmy,
                            target_unit_id = TargetUnit},
    
    packet:send(Data#data.socket, Target),
    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({'UPDATE_PERCEPTION'}, _From, Data) ->
    
    
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

