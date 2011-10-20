-module(data).
-export([load/0]).

-include_lib("xmerl/include/xmerl.hrl").
-include("schema.hrl").
-include("common.hrl").

load() ->
    {R, _} = xmerl_scan:file("items.xml"),

    extract(R, []).

extract(R, List) when is_record(R, xmlElement) ->
    case R#xmlElement.name of
        row ->
            DataList = lists:reverse(lists:foldl(fun extract/2, [], R#xmlElement.content)),
            RecordDataList = [ item_type | DataList], 
            RecordData = list_to_tuple(RecordDataList),
            db:write(RecordData);
        _ ->
            lists:foldl(fun extract/2, List, R#xmlElement.content)
    end;

extract(#xmlText{parents=[{id,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract(#xmlText{parents=[{item_type, _}, {row, _}, _], value=V}, L) ->
    [ V | L];
extract(#xmlText{parents=[{name, _}, {row, _}, _], pos=1, value=V}, L) ->
    [ V | L];
extract(#xmlText{parents=[{production_cost, _}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract(#xmlText{parents=[{batch_amount, _}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract(#xmlText{parents=[{buildin_req, _}, {row, _}, _], value=V}, L) ->
    [ V | L];
extract(#xmlText{parents=[{improvement_req, _}, {row, _}, _], value=V}, L) ->
    [ V | L];
extract(#xmlText{parents=[{produces, _}, {row, _}, _], value=V}, L) ->
    [ convert_to_list(V) | L];
extract(#xmlText{parents=[{material_amount, _}, {row, _}, _], value=V}, L) ->
    [ convert_to_list(V) | L];
extract(#xmlText{parents=[{material_type, _}, {row, _}, _], value=V}, L) ->
    [ convert_to_list(V) | L];
extract(XMLTEXT, L) ->
    L.

convert_to_int("None") ->
    -1;
convert_to_int(V) ->
    {Int, []} = string:to_integer(V),
    Int.

convert_to_list("None") ->
    [];
convert_to_list(V) ->
    ListOfStrings = string:tokens(V, ";"),
    ListOfNums = lists:map(fun convert_to_int/1, ListOfStrings),
    ListOfNums.
