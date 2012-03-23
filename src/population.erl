%% Author: Peter
%% Created: Sep 26, 2009
%% Description: TODO: Add description to population
-module(population).

-behaviour(gen_server).
%%
%% Include files
%%
-include("schema.hrl").
-include("common.hrl").
%%
%% Exported Functions
%%
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([remove/4, update/4, get_by_city/1]).
-export([tuple_form/1]).
-export([check_population/4]).

%%
%% API Functions
%%

start() ->
    gen_server:start({global, population_pid}, population, [], []).

remove(City, Caste, Race, Amount) ->
    update(City, Caste, Race, -1 * Amount).

update(City, Caste, Race, Amount) ->
    gen_server:cast({global, population_pid}, {'UPDATE', City, Caste, Race, Amount}). 

get_by_city(CityId) ->
    gen_server:call({global, population_pid}, {'GET_BY_CITY', CityId}).

tuple_form(CityId) ->
    Populations = get_by_city(CityId),

    F = fun(Population, PopulationList) ->
        PopulationTuple = {Population#population.city_id,
                           Population#population.caste,
                           Population#population.race,
                           erlang:trunc(Population#population.value)},
        [PopulationTuple | PopulationList]
    end,
    lists:foldl(F, [], Populations).

check_population(CityId, Caste, Race, Amount) ->
    NumUnassigned = num_unassigned_caste(CityId, Race, Caste),
    Result = Amount =< NumUnassigned,
    Result.
      
num_unassigned_caste(CityId, Race, Caste) ->
    case db:dirty_read(population, {CityId, Caste, Race}) of
        [Population] ->
            AssignmentList = db:dirty_match_object({assignment, '_', CityId, Caste, Race, '_', '_'}),
            
            F = fun(Assignment, Total) ->
                    Total + Assignment#assignment.amount
            end,

            TotalAssigned = lists:foldl(F, 0, AssignmentList),
            Result = Population#population.value - TotalAssigned;
        _ ->
            Result = 0
    end,
    Result.
    

%get_race(CityId, Race) ->
%    Population = db:dirty_match_object({population, '_', CityId, '_', Race, '_'}),
    



%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    {ok, []}.

handle_cast({'UPDATE', City, Caste, Race, Amount}, Data) ->
    population_update(City, Caste, Race, Amount),
    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({'GET_BY_CITY', CityId}, _From, Data) ->
    Population = db:dirty_index_read(population, CityId, #population.city_id),
    {reply, Population, Data};

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

%%
%% Local Functions
%%

population_update(City, Caste, Race, Amount) ->
    [Population] = db:dirty_read(population, {City, Caste, Race}),
    CurrentAmount = Population#population.value,
    NewAmount = CurrentAmount + Amount,
    
    case NewAmount > 0 of
        true ->
            NewPopulation = Population#population { value = NewAmount},
            db:dirty_write(NewPopulation);
        false ->
            db:dirty_delete(population, Population)
    end. 
