-module(map_port).
-export([start/0, stop/0]).
-export([load/0, get/1, get_resources/1]).

start() ->
    start("port_drv").

start(SharedLib) ->
    case erl_ddll:load_driver(".", SharedLib) of
	ok -> ok;
	{error, already_loaded} -> ok;
	_ -> exit({error, could_not_load_driver})
    end,
    spawn(fun() -> init(SharedLib) end).

init(SharedLib) ->
    register(map_port, self()),
    Port = open_port({spawn, SharedLib}, []),
    loop(Port).

stop() ->
    map_port ! stop.

load() -> call_port({load}).
get(X) -> call_port({get, X}).
get_resources(X) -> call_port({get_resources, X}).

call_port(Msg) ->
    io:fwrite("Msg: ~w~n", [Msg]),
	io:fwrite("call: ~w self: ~w Msg: ~w~n", [call, self(), Msg]),
    map_port ! {call, self(), Msg},
	
    receive
		{map_port, Result} ->
			io:fwrite("Result: ~w~n", [Result]),
			Result;
		Any -> 
			io:fwrite("Receive Error: ~w~n" + [Any])
    end.

loop(Port) ->
    receive
	{call, Caller, Msg} ->
	    Port ! {self(), {command, encode(Msg)}},
	    receive
			{Port, {data, Data}} ->
				io:fwrite("Port Data: ~w~n", [Data]),
				Caller ! {map_port, decode(Data)};
			Any -> 
				io:fwrite("Port Receive Error: ~w~n", [Any])		
	    end,
	    loop(Port);
	stop ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    exit(normal)
	    end;
	{'EXIT', Port, Reason} ->
	    io:format("~p ~n", [Reason]),
	    exit(port_terminated);
	Any ->
		io:fwrite("Loop Receive Error: ~w~n", [Any])		
    end.

encode({load}) -> [1];
encode({get, X}) -> [2, <<X:16>>];
encode({get_resources, X}) -> [3, <<X:16>>].

decode([Value]) -> Value;
decode(Value) -> Value.

