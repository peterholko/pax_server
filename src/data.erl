-module(data).
-export([load/0]).

-include_lib("xmerl/include/xmerl.hrl").
-include("schema.hrl").
-include("common.hrl").

load() ->
    {ItemType, _} = xmerl_scan:file("item_type.xml"),
    {ItemCategory, _} = xmerl_scan:file("item_category.xml"),
    {ResourceType, _} = xmerl_scan:file("resource_type.xml"),

    extract_item_type(ItemType, []),
    extract_item_category(ItemCategory, []),
    extract_resource_type(ResourceType, []).

extract_item_type(R, List) when is_record(R, xmlElement) ->
    case R#xmlElement.name of
        row ->
            DataList = lists:reverse(lists:foldl(fun extract_item_type/2, [], R#xmlElement.content)),
            RecordDataList = [ item_type | DataList], 
            RecordData = list_to_tuple(RecordDataList),
            db:write(RecordData);
        _ ->
            lists:foldl(fun extract_item_type/2, List, R#xmlElement.content)
    end;

extract_item_type(#xmlText{parents=[{id,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_item_type(#xmlText{parents=[{item_type, _}, {row, _}, _], value=V}, L) ->
    [ V | L];
extract_item_type(#xmlText{parents=[{name, _}, {row, _}, _], pos=1, value=V}, L) ->
    [ V | L];
extract_item_type(#xmlText{parents=[{production_cost, _}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_item_type(#xmlText{parents=[{batch_amount, _}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_item_type(#xmlText{parents=[{buildin_req, _}, {row, _}, _], value=V}, L) ->
    [ V | L];
extract_item_type(#xmlText{parents=[{improvement_req, _}, {row, _}, _], value=V}, L) ->
    [ V | L];
extract_item_type(#xmlText{parents=[{produces, _}, {row, _}, _], value=V}, L) ->
    [ convert_to_list(V) | L];
extract_item_type(#xmlText{parents=[{material_amount, _}, {row, _}, _], value=V}, L) ->
    [ convert_to_list(V) | L];
extract_item_type(#xmlText{parents=[{material_type, _}, {row, _}, _], value=V}, L) ->
    [ convert_to_list(V) | L];
extract_item_type(_XMLTEXT, L) ->
    L.

extract_item_category(R, List) when is_record(R, xmlElement) ->
    case R#xmlElement.name of
        row ->
            DataList = lists:reverse(lists:foldl(fun extract_item_category/2, [], R#xmlElement.content)),
            RecordDataList = [ item_category | DataList], 
            RecordData = list_to_tuple(RecordDataList),
            db:write(RecordData);
        _ ->
            lists:foldl(fun extract_item_category/2, List, R#xmlElement.content)
    end;

extract_item_category(#xmlText{parents=[{item_category_id,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_item_category(#xmlText{parents=[{name, _}, {row, _}, _], pos=1, value=V}, L) ->
    [ V | L];
extract_item_category(#xmlText{parents=[{contains, _}, {row, _}, _], value=V}, L) ->
    [ convert_to_list(V) | L];
extract_item_category(_XMLTEXT, L) ->
    L.

extract_resource_type(R, List) when is_record(R, xmlElement) ->
    case R#xmlElement.name of
        row ->
            DataList = lists:reverse(lists:foldl(fun extract_resource_type/2, [], R#xmlElement.content)),
            RecordDataList = [ resource_type | DataList], 
            RecordData = list_to_tuple(RecordDataList),
            db:write(RecordData);
        _ ->
            lists:foldl(fun extract_resource_type/2, List, R#xmlElement.content)
    end;


extract_resource_type(#xmlText{parents=[{id,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_resource_type(#xmlText{parents=[{name,_}, {row, _}, _], value=V}, L) ->
    [ V | L];
extract_resource_type(_XMLTEXT, L) ->
    L.


convert_to_int("None") ->
    -1;
convert_to_int(V) ->
    case string:to_integer(V) of
        {Int, []} ->
            Return = Int;
        _Error ->
            ?ERROR("string:to_integer failure", V),
            Return = -1
    end, 
    Return.

convert_to_list("None") ->
    [];
convert_to_list(V) ->
    ListOfStrings = string:tokens(V, ";"),
    StrippedList = lists:map(fun string:strip/1, ListOfStrings),
    ListOfNums = lists:map(fun convert_to_int/1, StrippedList),
    ListOfNums.
