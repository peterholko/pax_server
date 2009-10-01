%% Author: Peter
%% Created: Sep 13, 2009
%% Description: TODO: Add description to improvements
-module(improvements).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([get_improvements/1]).

%%
%% API Functions
%%

get_improvements([]) ->
	[];

get_improvements(TileList) ->
	list_improvements([], TileList).	

%%
%% Local Functions
%%

list_improvements(ImprovementList, []) ->
	ImprovementList;

list_improvements(ImprovementList, TileList) ->
	[TileIndex | Rest] = TileList,
	TileImprovements = db:dirty_read(improvement, TileIndex),	
	NewImprovementList = lists:append(ImprovementList, TileImprovements),
	
	list_improvements(NewImprovementList, Rest).
