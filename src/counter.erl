%% Author: Peter
%% Created: Feb 15, 2009
%% Description: TODO: Add description to counter
-module(counter).

-include("schema.hrl").

-export([increment/1, increment/2, reset/1]).

increment(Type) ->
    increment(Type, 1).

increment(Type, Value) ->
    mnesia:dirty_update_counter(counter, Type, Value).    

reset(entity) ->
    Counter = #counter{
                       type = entity,
                       value = 20
                      },
    ok = db:write(Counter);
reset(Type) ->
    Counter = #counter{
                       type = Type,
                       value = 0
                      },
    ok = db:write(Counter).

