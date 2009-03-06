%% Author: Peter
%% Created: Feb 4, 2009
%% Description: TODO: Add description to army
-module(army).
-behaviour(gen_server).

%%
%% Include files
%%

-include("common.hrl").
-include("schema.hrl").

%%
%% Exported Functions
%%
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-export([start/2, stop/1]).

-record(module_data, {
          army_id,    
          player_id, 
          self
         }).

%%
%% API Functions
%%

start(ArmyId, PlayerId) ->
	gen_server:start({global, {army, ArmyId}}, army, [ArmyId, PlayerId], []).

init([ArmyId, PlayerId]) 
  when is_integer(ArmyId),
       is_integer(PlayerId) ->
    process_flag(trap_exit, true),
    io:fwrite("army - army_id: ~w player_id: ~w~n", [ArmyId, PlayerId]),
    {ok, #module_data{ army_id = ArmyId, player_id = PlayerId, self = self() }}.



terminate(_Reason, _) ->
    ok.

stop(ProcessId) 
  when is_pid(ProcessId) ->
    gen_server:cast(ProcessId, stop).

handle_cast({'SET_STATE_MOVE', DestX, DestY}, Data) ->
    io:fwrite("army - set_state_move ~n"),
    
    [Army] = db:read(army, Data#module_data.army_id),
    ArmySpeed = get_army_speed(Data#module_data.army_id),
    
    if
        Army#army.state =/= ?STATE_MOVE ->
            gen_server:cast(global:whereis_name(game_pid), {'CLEAR_EVENTS', Data#module_data.self}),
        	gen_server:cast(global:whereis_name(game_pid), {'ADD_EVENT', Data#module_data.self, ?EVENT_MOVE, speed_to_ticks(ArmySpeed)});
        true ->
            ok
    end,         
    
    db_state_move(Data#module_data.army_id, DestX, DestY),  
    
    {noreply, Data};

handle_cast({'SET_STATE_ATTACK', TargetId}, Data) ->
    io:fwrite("army - set_state_attack ~n"),
    [Army] = db:read(army, Data#module_data.army_id),   
    ArmySpeed = get_army_speed(Data#module_data.army_id),
    
   	if
        Army#army.state =/= ?STATE_ATTACK ->
            gen_server:cast(global:whereis_name(game_pid), {'CLEAR_EVENTS', Data#module_data.self}),
        	gen_server:cast(global:whereis_name(game_pid), {'ADD_EVENT', Data#module_data.self, ?EVENT_ATTACK, speed_to_ticks(ArmySpeed)});
        true ->
            ok
    end,         
    
    
    db_state_attack(Data#module_data.army_id, TargetId), 
    
    {noreply, Data};

handle_cast({'SET_STATE_COMBAT'}, Data) ->
    
    gen_server:cast(global:whereis_name(game_pid), {'CLEAR_EVENTS', Data#module_data.self}),
    db_state_combat(Data#module_data.army_id),
    
    {noreply, Data};

handle_cast({'SET_STATE_NONE'}, Data) ->

	db_state_none(Data#module_data.army_id),

	{noreply, Data};	

handle_cast({'PROCESS_EVENT', EventType}, Data) ->
    
    case EventType of
		?EVENT_MOVE ->
            do_move(Data#module_data.army_id, Data#module_data.player_id, Data#module_data.self);
        ?EVENT_ATTACK ->
            do_attack(Data#module_data.army_id, Data#module_data.player_id, Data#module_data.self);
        ?EVENT_NONE ->
            ok
    end,      
    
    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({'GET_INFO'}, _From, Data) ->
	{reply, Data#module_data.player_id, Data};

handle_call({'GET_STATE'}, _From, Data) ->
    [Army] = db:read(army, Data#module_data.army_id),
    
	{reply, {Army#army.id, Army#army.player_id, 0, Army#army.state, Army#army.x, Army#army.y}, Data};

handle_call({'GET_ARMY_ID'}, _From, Data) ->
	{reply, Data#module_data.army_id, Data};

handle_call({'GET_PLAYER_ID'}, _From, Data) ->
	{reply, Data#module_data.player_id, Data};

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
%%
%% Local Functions
%%

do_move(ArmyId, PlayerId, ArmyPid) ->
    [Army] = db:read(army, ArmyId),
    
    ArmyX = Army#army.x,
    ArmyY = Army#army.y,
    DestX = Army#army.dest_x,
    DestY = Army#army.dest_y,
    
    DiffX =  DestX - ArmyX,
    DiffY =  DestY - ArmyY,
    
    if
        DiffX > 0 ->
            NewArmyX = ArmyX + 1;
        DiffX < 0 ->
            NewArmyX = ArmyX - 1;
        true ->
            NewArmyX = ArmyX,
            ok
	end,

    if
        DiffY > 0 ->
            NewArmyY = ArmyY + 1;
        DiffY < 0 ->
            NewArmyY = ArmyY - 1;
        true ->
            NewArmyY = ArmyY,
            ok
    end,
          
    Guard1 = (NewArmyX =:= DestX) and (NewArmyY =:= DestY),
    
    io:fwrite("army - do_move - NewArmyX: ~w ArmyX: ~w NewArmyY: ~w ArmyY: ~w Guard1: ~w~n", [NewArmyX, ArmyX, NewArmyY, ArmyY, Guard1]),
    gen_server:cast(global:whereis_name({player, PlayerId}), {'SET_DISCOVERED_TILES', ArmyId, NewArmyX, NewArmyY}),
    
	if	
        Guard1 ->
            db_state_none(ArmyId, NewArmyX, NewArmyY);
		true ->
            db_event_move(ArmyId, NewArmyX, NewArmyY),
            ArmySpeed = get_army_speed(ArmyId),
			gen_server:cast(global:whereis_name(game_pid), {'ADD_EVENT', ArmyPid, ?EVENT_MOVE, speed_to_ticks(ArmySpeed)})

	end.

do_attack(ArmyId, PlayerId, ArmyPid) ->
    [Army] = db:read(army, ArmyId),
    
    ArmyX = Army#army.x,
    ArmyY = Army#army.y,
    TargetId = Army#army.target,
    
    {_, _, _, _, TargetX, TargetY} = gen_server:call(global:whereis_name({army, TargetId}), {'GET_STATE'}),
    
    DiffX = TargetX - ArmyX,
    DiffY = TargetY - ArmyY,
    
    if
        DiffX > 0 ->
            NewArmyX = ArmyX + 1;
        DiffX < 0 ->
            NewArmyX = ArmyX - 1;
        true ->
            NewArmyX = ArmyX,
            ok
	end,

    if
        DiffY > 0 ->
            NewArmyY = ArmyY + 1;
        DiffY < 0 ->
            NewArmyY = ArmyY - 1;
        true ->
            NewArmyY = ArmyY,
            ok
    end,
          
    Guard1 = (NewArmyX =:= TargetX) and (NewArmyY =:= TargetY),
    
    io:fwrite("army - do_attack - NewArmyX: ~w NewArmyY: ~w~n", [NewArmyX, NewArmyY]),
    gen_server:cast(global:whereis_name({player, PlayerId}), {'SET_DISCOVERED_TILES', ArmyId, NewArmyX, NewArmyY}),
    
	if	
        Guard1 ->
            gen_server:cast(global:whereis_name({army, TargetId}), {'SET_STATE_COMBAT'}),
            db_state_combat(ArmyId, NewArmyX, NewArmyY);
		true ->
            db_event_move(ArmyId, NewArmyX, NewArmyY),
            ArmySpeed = get_army_speed(ArmyId),
			gen_server:cast(global:whereis_name(game_pid), {'ADD_EVENT', ArmyPid, ?EVENT_ATTACK, speed_to_ticks(ArmySpeed)})

	end.      

db_event_move(ArmyId, NewX, NewY) ->
    io:fwrite("army - db_event_move~n"),
    F = fun() ->
                [Army] = mnesia:read(army, ArmyId, write),
                NewArmy = Army#army{x = NewX, 
                                    y = NewY},
                mnesia:write(NewArmy)
		end,
	mnesia:transaction(F).    

db_state_move(ArmyId, DestX, DestY) ->
    F = fun() ->
                [Army] = mnesia:read(army, ArmyId, write),
                NewArmy = Army#army{dest_x = DestX, 
                                    dest_y = DestY,
                                    state = ?STATE_MOVE},
                mnesia:write(NewArmy)
		end,
	mnesia:transaction(F).

db_state_attack(ArmyId, TargetId) ->
    F = fun() ->
                [Army] = mnesia:read(army, ArmyId, write),
                NewArmy = Army#army{state = ?STATE_ATTACK,
                                    target = TargetId},
                mnesia:write(NewArmy)
		end,
	mnesia:transaction(F). 

db_state_combat(ArmyId) ->
    F = fun() ->
                [Army] = mnesia:read(army, ArmyId, write),
                NewArmy = Army#army{state = ?STATE_COMBAT},
                mnesia:write(NewArmy)
		end,
	mnesia:transaction(F). 

db_state_combat(ArmyId, X, Y) ->
    F = fun() ->
                [Army] = mnesia:read(army, ArmyId, write),
                NewArmy = Army#army{state = ?STATE_COMBAT,
                                    x = X,
                                    y = Y},
                mnesia:write(NewArmy)
		end,
	mnesia:transaction(F). 

db_state_none(ArmyId, X, Y) ->
    F = fun() ->
                [Army] = mnesia:read(army, ArmyId, write),
                NewArmy = Army#army{state = ?STATE_NONE,
                                    x = X,
                                    y = Y},
                mnesia:write(NewArmy)
		end,
	mnesia:transaction(F).   

db_state_none(ArmyId) ->
    F = fun() ->
                [Army] = mnesia:read(army, ArmyId, write),
                NewArmy = Army#army{state = ?STATE_NONE},
                mnesia:write(NewArmy)
		end,
	mnesia:transaction(F). 

get_army_speed(ArmyId) ->
    %UnitsSpeed = unit:units_speed(ArmyId),
    UnitsSpeed = 5.
    %lists:max(UnitsSpeed).

speed_to_ticks(Speed) ->
    Speed * (1000 div ?GAME_LOOP).
    
    