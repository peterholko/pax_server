-module(data).
-export([load/0]).

-include_lib("xmerl/include/xmerl.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("schema.hrl").
-include("common.hrl").

load() ->
    {ItemCategory, _} = xmerl_scan:file("item_category.xml"),
    {ResourceType, _} = xmerl_scan:file("resource_type.xml"),
    {ImprovementType, _} = xmerl_scan:file("improvement_type.xml"),
    {BuildingType, _} = xmerl_scan:file("building_type.xml"),
    {ItemBase, _} = xmerl_scan:file("item_base.xml"),
    {ItemTemplate, _} = xmerl_scan:file("item_template.xml"),
    {UnitTemplate, _} = xmerl_scan:file("unit_template.xml"),

    extract_item_category(ItemCategory, []),
    extract_resource_type(ResourceType, []),
    extract_improvement_type(ImprovementType, []),
    extract_building_type(BuildingType, []),
    extract_item_base(ItemBase, []),
    extract_item_template(ItemTemplate, []),
    extract_unit_template(UnitTemplate, []).

extract_item_base(R, List) when is_record(R, xmlElement) ->
    case R#xmlElement.name of
        row ->
            DataList = lists:reverse(lists:foldl(fun extract_item_base/2, [], R#xmlElement.content)),
            ProcessDataList = process_item_base(DataList),
            RecordDataList = [ item_base | ProcessDataList],
            RecordData = list_to_tuple(RecordDataList),
            db:write(RecordData);
        _ ->
            lists:foldl(fun extract_item_base/2, List, R#xmlElement.content)
    end;

extract_item_base(#xmlText{parents=[{type_id,_}, {row, _}, _], value=V}, L) ->
    [ {type_id, convert_to_int(V)} | L];
extract_item_base(#xmlText{parents=[{name, _}, {row, _}, _], value=V}, L) ->
    [ {name, V} | L];
extract_item_base(#xmlText{parents=[{category, _}, {row, _}, _], pos=1, value=V}, L) ->
    [ {category, V} | L];
extract_item_base(#xmlText{parents=[{production_cost, _}, {row, _}, _], value=V}, L) ->
    [ {production_cost, convert_to_int(V)} | L];
extract_item_base(#xmlText{parents=[{batch_amount, _}, {row, _}, _], value=V}, L) ->
    [ {batch_amount, convert_to_int(V)} | L];
extract_item_base(#xmlText{parents=[{building_req, _}, {row, _}, _], value=V}, L) ->
    [ {building_req, V} | L];
extract_item_base(#xmlText{parents=[{building_level, _}, {row, _}, _], value=V}, L) ->
    [ {building_level, convert_to_int(V)} | L];
extract_item_base(#xmlText{parents=[{improvement_req, _}, {row, _}, _], value=V}, L) ->
    [ {improvement_req, V} | L];
extract_item_base(#xmlText{parents=[{improvement_level, _}, {row, _}, _], value=V}, L) ->
    [ {improvement_level, convert_to_int(V)} | L];
extract_item_base(#xmlText{parents=[{produces, _}, {row, _}, _], value=V}, L) ->
    [ {produces, convert_to_list(V)} | L];
extract_item_base(#xmlText{parents=[{material_amount, _}, {row, _}, _], value=V}, L) ->
    [ {material_amount, convert_to_int(V)} | L];
extract_item_base(#xmlText{parents=[{material_type, _}, {row, _}, _], value=V}, L) ->
    [ {material_type, convert_to_int(V)} | L];
extract_item_base(_XMLTEXT, L) ->
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

extract_item_category(#xmlText{parents=[{id,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_item_category(#xmlText{parents=[{name, _}, {row, _}, _], pos=1, value=V}, L) ->
    [ list_to_atom(V) | L];
extract_item_category(#xmlText{parents=[{display_name, _}, {row, _}, _], pos=1, value=V}, L) ->
    [ V | L];
extract_item_category(#xmlText{parents=[{contains, _}, {row, _}, _], value=V}, L) ->
    [ convert_to_list(V) | L];
extract_item_category(_XMLTEXT, L) ->
    L.

extract_item_template(R, List) when is_record(R, xmlElement) ->
    case R#xmlElement.name of
        row ->
            DataList = lists:reverse(lists:foldl(fun extract_item_template/2, [], R#xmlElement.content)),
            ProcessDataList = process_item_template(DataList),
            RecordDataList = [ item_template | ProcessDataList], 
            RecordData = list_to_tuple(RecordDataList),
            db:write(RecordData);
        _ ->
            lists:foldl(fun extract_item_template/2, List, R#xmlElement.content)
    end;

extract_item_template(#xmlText{parents=[{id,_}, {row, _}, _], value=V}, L) ->
    [ {id, convert_to_int(V)} | L];
extract_item_template(#xmlText{parents=[{category, _}, {row, _}, _], value=V}, L) ->
    [ {category, V}| L];
extract_item_template(#xmlText{parents=[{name, _}, {row, _}, _], value=V}, L) ->
    [ {name, V} | L];
extract_item_template(#xmlText{parents=[{production_cost, _}, {row, _}, _], value=V}, L) ->
    [ {production_cost, convert_to_int(V)} | L];
extract_item_template(#xmlText{parents=[{batch_amount, _}, {row, _}, _], value=V}, L) ->
    [ {batch_amount, convert_to_int(V)} | L];
extract_item_template(#xmlText{parents=[{building_req, _}, {row, _}, _], value=V}, L) ->
    [ {building_req, V} | L];
extract_item_template(#xmlText{parents=[{building_level, _}, {row, _}, _], value=V}, L) ->
    [ {building_level, convert_to_int(V)} | L];
extract_item_template(#xmlText{parents=[{material_amount, _}, {row, _}, _], value=V}, L) ->
    [ {material_amount, convert_to_list(V)} | L];
extract_item_template(#xmlText{parents=[{material_category, _}, {row, _}, _], value=V}, L) ->
    [ {material_category, convert_to_list(V)} | L];
extract_item_template(_XMLTEXT, L) ->
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

extract_improvement_type(R, List) when is_record(R, xmlElement) ->
    case R#xmlElement.name of
        row ->
            DataList = lists:reverse(lists:foldl(fun extract_improvement_type/2, [], R#xmlElement.content)),
            RecordDataList = [ improvement_type | DataList], 
            RecordData = list_to_tuple(RecordDataList),
            db:write(RecordData);
        _ ->
            lists:foldl(fun extract_improvement_type/2, List, R#xmlElement.content)
    end;

extract_improvement_type(#xmlText{parents=[{id,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_improvement_type(#xmlText{parents=[{imp_type,_}, {row, _}, _], value=V}, L) ->
    [ V | L];
extract_improvement_type(#xmlText{parents=[{imp_level,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_improvement_type(#xmlText{parents=[{name,_}, {row, _}, _], value=V}, L) ->
    [ V | L];
extract_improvement_type(#xmlText{parents=[{total_hp,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_improvement_type(#xmlText{parents=[{population_cap,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_improvement_type(#xmlText{parents=[{production_cost,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_improvement_type(#xmlText{parents=[{gold_cost,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_improvement_type(#xmlText{parents=[{lumber_cost,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_improvement_type(#xmlText{parents=[{stone_cost,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_improvement_type(#xmlText{parents=[{upkeep,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_improvement_type(_XMLTEXT, L) ->
    L.

extract_building_type(R, List) when is_record(R, xmlElement) ->
    case R#xmlElement.name of
        row ->
            RecordFieldList = record_info(fields, building_type),
            DataList = lists:foldl(fun extract_building_type/2, [], R#xmlElement.content),
            OrderedList = order_tagged_list(RecordFieldList, DataList),
            RecordDataList = [ building_type | OrderedList], 
            RecordData = list_to_tuple(RecordDataList),
            db:write(RecordData);
        _ ->
            lists:foldl(fun extract_building_type/2, List, R#xmlElement.content)
    end;

extract_building_type(#xmlText{parents=[{id,_}, {row, _}, _], value=V}, L) ->
    [ {id, convert_to_int(V)} | L];
extract_building_type(#xmlText{parents=[{building_type, _}, {row, _}, _], value=V}, L) ->
    [ {building_type, V} | L];
extract_building_type(#xmlText{parents=[{level, _}, {row, _}, _], value=V}, L) ->
    [ {level, convert_to_int(V)} | L];
extract_building_type(#xmlText{parents=[{name, _}, {row, _}, _], value=V}, L) ->
    [ {name, V} | L];
extract_building_type(#xmlText{parents=[{hp, _}, {row, _}, _], value=V}, L) ->
    [ {hp, convert_to_int(V)} | L];
extract_building_type(#xmlText{parents=[{population_cap, _}, {row, _}, _], value=V}, L) ->
    [ {population_cap, convert_to_int(V)} | L];
extract_building_type(#xmlText{parents=[{production_cost, _}, {row, _}, _], value=V}, L) ->
    [ {production_cost, convert_to_int(V)} | L];
extract_building_type(#xmlText{parents=[{gold_cost, _}, {row, _}, _], value=V}, L) ->
    [ {gold_cost, convert_to_int(V)} | L];
extract_building_type(#xmlText{parents=[{lumber_cost, _}, {row, _}, _], value=V}, L) ->
    [ {lumber_cost, convert_to_int(V)} | L];
extract_building_type(#xmlText{parents=[{stone_cost, _}, {row, _}, _], value=V}, L) ->
    [ {stone_cost, convert_to_int(V)} | L];
extract_building_type(#xmlText{parents=[{upkeep, _}, {row, _}, _], value=V}, L) ->
    [ {upkeep, convert_to_int(V)} | L];
extract_building_type(_XMLTEXT, L) ->
    L.

order_tagged_list(RecordFieldList, TaggedList) ->
    F = fun(E) ->
            {_Name, V} = lists:keyfind(E, 1, TaggedList),
            V
       end,
    lists:map(F, RecordFieldList).

extract_unit_template(R, List) when is_record(R, xmlElement) ->
    case R#xmlElement.name of
        row ->
            DataList = lists:reverse(lists:foldl(fun extract_unit_template/2, [], R#xmlElement.content)),
            RecordDataList = [unit_template | DataList],
            RecordData = list_to_tuple(RecordDataList),
            db:write(RecordData);
        _ ->
            lists:foldl(fun extract_unit_template/2, List, R#xmlElement.content)
    end;

extract_unit_template(#xmlText{parents=[{id,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_unit_template(#xmlText{parents=[{name,_}, {row, _}, _], value=V}, L) ->
    [  list_to_binary(V) | L];
extract_unit_template(#xmlText{parents=[{level,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_unit_template(#xmlText{parents=[{type,_}, {row, _}, _], value=V}, L) ->
    [ V | L];
extract_unit_template(#xmlText{parents=[{building_req,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_unit_template(#xmlText{parents=[{hp,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_unit_template(#xmlText{parents=[{atk,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_unit_template(#xmlText{parents=[{def,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_unit_template(#xmlText{parents=[{range,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_unit_template(#xmlText{parents=[{speed,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_unit_template(#xmlText{parents=[{acc,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_unit_template(#xmlText{parents=[{eva,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_unit_template(#xmlText{parents=[{effect_amount,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_unit_template(#xmlText{parents=[{effect_type,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_unit_template(#xmlText{parents=[{production_cost,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_unit_template(#xmlText{parents=[{gold_cost,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_unit_template(#xmlText{parents=[{material_amount,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_list(V) | L];
extract_unit_template(#xmlText{parents=[{material_category,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_list(V) | L];
extract_unit_template(#xmlText{parents=[{upkeep,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_unit_template(#xmlText{parents=[{food,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_unit_template(#xmlText{parents=[{capacity,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_unit_template(#xmlText{parents=[{def_size,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_unit_template(#xmlText{parents=[{min_size,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_unit_template(#xmlText{parents=[{max_size,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_unit_template(#xmlText{parents=[{movement,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_unit_template(#xmlText{parents=[{value,_}, {row, _}, _], value=V}, L) ->
    [ convert_to_int(V) | L];
extract_unit_template(_XMLTEXT, L) ->
    L.

convert_to_int("None") ->
    -1;
convert_to_int(V) ->
    case string:to_integer(V) of
        {Int, []} ->
            Return = Int;
        _Error ->
            io:fwrite("~w~n", [V]),
            ?ERROR("string:to_integer failure", V),
            Return = -1
    end, 
    Return.
convert_to_list("") ->
    [];
convert_to_list("None") ->
    [];
convert_to_list(V) ->
    ListOfStrings = string:tokens(V, ";"),
    StrippedList = lists:map(fun string:strip/1, ListOfStrings),
    ListOfNums = lists:map(fun convert_to_int/1, StrippedList),
    ListOfNums.

convert_to_list_of_strings(V) ->
    ListOfStrings = string:tokens(V, ";"),
    StrippedList = lists:map(fun string:strip/1, ListOfStrings),
    StrippedList.

process_item_base(ItemBaseList) ->
    BuildingReqName = element(2, lists:keyfind(building_req, 1, ItemBaseList)),
    BuildingReqLevel = element(2, lists:keyfind(building_level, 1, ItemBaseList)),
    ImproveReqName = element(2, lists:keyfind(improvement_req, 1, ItemBaseList)),
    ImproveReqLevel = element(2, lists:keyfind(improvement_level, 1, ItemBaseList)),

    case building:match_req(BuildingReqName, BuildingReqLevel) of
        {true, BuildingType} ->
            BuildingReqId = BuildingType#building_type.id;
        false ->
            BuildingReqId = ?ITEM_REQ_NONE
    end,
      
    case improvement:match_req(ImproveReqName, ImproveReqLevel) of
        {true, ImprovementType} ->
            ImprovementReqId = ImprovementType#improvement_type.id;
        false ->
            ImprovementReqId = ?ITEM_REQ_NONE
    end,

    List1 = lists:keyreplace(building_req, 1, ItemBaseList, {building_req, BuildingReqId}),
    List2 = lists:keyreplace(improvement_req, 1, List1, {improvement_req, ImprovementReqId}),
    List3 = lists:keydelete(building_level, 1, List2),
    List4 = lists:keydelete(improvement_level, 1, List3),

    {_Tags, Values} = lists:unzip(List4),
    Values.
    
process_item_template(ItemTemplateList) ->
    %?INFO("ItemTemplateList: ", ItemTemplateList),
    BuildingReqName = element(2, lists:keyfind(building_req, 1, ItemTemplateList)),
    BuildingReqLevel = element(2, lists:keyfind(building_level, 1, ItemTemplateList)),

    %?INFO("BuildingReqName: ", BuildingReqName),
    %?INFO("BuildingReqLevel: ", BuildingReqLevel),
   
    case building:match_req(BuildingReqName, BuildingReqLevel) of
        {true, BuildingType} ->
            BuildingReqId = BuildingType#building_type.id;
        false ->
            BuildingReqId = ?ITEM_REQ_NONE
    end,
    List1 = lists:keyreplace(building_req, 1, ItemTemplateList, {building_req, BuildingReqId}),
    List2 = lists:keydelete(building_level, 1, List1),

    {_Tags, Values} = lists:unzip(List2),
    Values.

process_unit_template(UnitTemplateList) ->
    BuildingReqName = element(2, lists:keyfind(building_req, 1, UnitTemplateList)),
    BuildingReqLevel = element(2, lists:keyfind(building_level, 1, UnitTemplateList)),

    case building:match_req(BuildingReqName, BuildingReqLevel) of
        {true, BuildingType} ->
            BuildingReqId = BuildingType#building_type.id;
        false ->
            BuildingReqId = ?ITEM_REQ_NONE
    end,
    List1 = lists:keyreplace(building_req, 1, UnitTemplateList, {building_req, BuildingReqId}),
    List2 = lists:keydelete(building_level, 1, List1),

    {_Tags, Values} = lists:unzip(List2),
    Values.
