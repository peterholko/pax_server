%% Author: Peter
%% Created: Dec 24, 2008
%% Description: TODO: Add description to util
-module(util).

%%
%% Include files
%%

-include("common.hrl").
-include("schema.hrl").

%%
%% Exported Functions
%%
-export([get_time/0,
         get_time_seconds/0,
         unique_list/1,
         is_process_alive/1, 
         reset_explored_map/0
        ]).

%%
%% API Functions
%%

unique_list(L) ->
    T = ets:new(temp,[set]),
    L1 = lists:filter(fun(X) -> ets:insert_new(T, {X,1}) end, L),
    ets:delete(T),
    L1.

is_process_alive(Pid) 
  when is_pid(Pid) ->
    rpc:call(node(Pid), erlang, is_process_alive, [Pid]).

get_time() ->
    {Megasec, Sec, Microsec} = erlang:now(),
    Milliseconds = (Megasec * 1000000000) + (Sec * 1000) + (Microsec div 1000),
    Milliseconds.

get_time_seconds() ->
    {Megasec, Sec, Microsec} = erlang:now(),
    Seconds = (Megasec * 1000000) + Sec,
    Seconds.

reset_explored_map() ->
    Players = db:select_all_players(),
    Armies = db:select_all_armies(),
    
    F1 = fun(PlayerInfo) ->
                PlayerId = PlayerInfo#player.id,
  				FileName = "map" ++ integer_to_list(PlayerId) ++ ".dets",
				{_,DetsFile} = dets:open_file(FileName,[{type, set}]),
                dets:delete_all_objects(DetsFile),
                dets:close(FileName)
        end,   
    
    lists:foreach(F1, Players),    
    
    F2 = fun(Element) ->
                PlayerId = Element#army.player_id,
                X = Element#army.x,
                Y = Element#army.y,
                TileIndexList = map:get_surrounding_tiles(X, Y),
                TileList = gen_server:call(global:whereis_name(map_pid), {'GET_EXPLORED_MAP', TileIndexList}),
  				FileName = "map" ++ integer_to_list(PlayerId) ++ ".dets",
				{_,DetsFile} = dets:open_file(FileName,[{type, set}]),
                
    			F3 = fun(TileInfo) ->
             			{TileIndex, _} = TileInfo,   
                		dets:insert(DetsFile, {TileIndex}) 
        		end,
              
    			lists:foreach(F3, TileList),
        		dets:close(FileName)
        end,
    
    lists:foreach(F2, Armies).


    
	