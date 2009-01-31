%% Author: Peter
%% Created: Dec 24, 2008
%% Description: TODO: Add description to util
-module(util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([is_process_alive/1, now_to_milliseconds/1]).

%%
%% API Functions
%%

is_process_alive(Pid) 
  when is_pid(Pid) ->
    rpc:call(node(Pid), erlang, is_process_alive, [Pid]).

now_to_milliseconds({Megasec, Sec, Microsec}) ->
    Milliseconds = (Megasec * 1000000000) + (Sec * 1000) + (Microsec div 1000),
    Milliseconds.