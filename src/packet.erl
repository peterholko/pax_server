% Author: Peter
%% Created: Dec 15, 2008
%% Description: TODO: Add description to packet
-module(packet).

%%
%% Include files
%%
-include("packet.hrl").

-import(pickle, [pickle/2, unpickle/2, byte/0, 
                 short/0, sshort/0, int/0, sint/0, 
                 long/0, slong/0, list/2, choice/2, 
                 optional/1, wrap/2, tuple/1, record/2, 
                 binary/1, string/0, wstring/0
                ]).

%%
%% Exported Functions
%%
-export([read/1, write/1, send/2, send_policy/1, send_clocksync/1, battle_joined_test/0, army_info_test/0, tt_test/0]).

%%
%% Local Functions
%%

-record(test, {id}).

test() ->
    record(test, {int()}).

name() ->
    string().

pass() ->
    string().

player() ->
    int().

id() ->
    int().

tile_index() ->
    int().

state() ->
    short().

type() ->
    short().

amount() ->
    int().

x() ->
    short().

y() ->
    short().

caste() ->
    byte().

resource() ->
    tuple({id(), type(), int(), int()}).

resources() ->
    list(short(), resource()).

entity() ->
    tuple({id(), player(), type(), type(), state(), x(), y()}).

entities() ->
    list(short(), entity()).

tile() ->
    tuple({tile_index(), byte()}).

tiles() ->
    list(int(), tile()). 

info_list() ->
    list(int(), string()).    

unit_id() ->
    id().

unit_size() ->
    int().

unit_type() ->
    int().

race() ->
    byte().

%building_id, hp, building_type
building() ->
    tuple({id(), int(), type()}).

buildings() ->
    list(short(), building()).

source_id() ->
    id().

source_type() ->
    type().

target_id() ->
    id().

target_type() ->
    type().

battle_id() ->
    id().

source_army_id() ->
    id().

source_unit_id() ->
    id().

target_army_id() ->
    id().

target_unit_id() ->
    id().

claim() ->
    tuple({id(), tile_index(), id(), state(), int()}).

claims() ->
    list(short(), claim()).

improvement() ->
    tuple({id(), type()}).

improvements() ->
    list(short(), improvement()).

%id, caste, race, amount, target_id, target_type
assignment() ->
    tuple({id(), byte(), byte(), int(), id(), short()}).

assignments() ->
    list(short(), assignment()).

%item id, owner_type, owner_id, player_id, item_type, item_template, item_volume
item() ->
    tuple({id(), byte(), id(), id(), type(), id(), int()}).

items() ->
    list(short(), item()).


gear() ->
    list(short(), id()).

%unit_id, recipe_id, unit_size, items
unit_items() ->
    tuple({unit_id(), id(), unit_size(), name(), gear(), items()}).

units() ->
    list(short(), unit_items()).

%unit_id, unit_name, template_id, unit_size
battle_unit() ->
    tuple({unit_id(), string(), id(), unit_size()}).

battle_units() ->
    list(short(), battle_unit()).

%army_id, player_id, army_name, kingdom_name, battle_units
army() ->
    tuple({id(), player(), name(), name(), battle_units()}).

armies() ->
    list(short(), army()).

%city_id, caste, race, value
population() ->
    tuple({id(), byte(), byte(), int()}).

populations() ->
    list(short(), population()).

%id, city_id, contract_type, target_type, target_id, object_type, production, created_time, update_time
contract() ->
    tuple({id(), id(), type(),  type(), id(), type(), int(), int(), int()}).

contracts() ->
    list(short(), contract()).

material() ->
    int().

materials() ->
    list(short(), material()).	

%type_id, template_id, player_id, item_name, flavour_text, material_maount, material_type
item_recipe() ->
    tuple({id(), id(), id(), string(), string(), materials(), materials()}).

item_recipes() ->
    list(short(), item_recipe()).

%type_id, template_id, player_id, unit_name, default_size, gear
unit_recipe() ->
    tuple({id(), id(), id(), string(), int(), gear()}).

unit_recipes() ->
    list(short(), unit_recipe()).
%tax_type, input_rate
tax() ->
    tuple({short(), short()}).

taxes() ->
    list(short(), tax()).

army_unit() ->
    tuple({id(), id()}).

%
% packet records
%

tt() ->
    record(tt, {test()}).

login() ->
    record(login, {name(),
                   pass()}).

bad() ->
    record(bad, {byte(),
                 byte()}).

success() ->
    record(success, {type(),
                     id()}).

chat_message() ->
    record(chat_message, {id(),
                          name(),
                          string()}).
player_id() ->
    record(player_id, {player()}).

move() ->
    record(move, {id(),
                  x(),
                  y()}).

attack() ->
    record(attack, {id(),
                    id()}).

add_waypoint() ->
    record(add_waypoint, {id(),
                          x(),
                          y()}).

perception() ->
    record(perception, {entities(),
                        tiles()}).

map() ->
    record(map, {tiles()}).


request_info() ->
    record(request_info, {short(),
                          id()}).

info() ->
    record(info, {info_list()}).

info_kingdom() ->
    record(info_kingdom, {id(), %kingdom_id 
                          name(), %name
                          int(), %gold
                          item_recipes(), %item_recipes
                          unit_recipes()}). %unit_recipes
info_army() ->
    record(info_army, {id(),
                       name(), %army_name
                       units()}). %units

info_city() ->
    record(info_city, {id(),
                       name(),
                       int(), %tax_commoner
                       int(), %tax_noble
                       int(), %tax_tariff
                       buildings(),
                       units(),
                       claims(),
                       improvements(),
                       assignments(),
                       items(),
                       populations(),
                       contracts()}).

info_tile() ->
    record(info_tile, {int(), %tile_index,
                       short(), %tile_type,
                       resources(),
                       claim()}).

info_generic_army() ->
    record(info_generic_army, {id(),   
                               id(), %player id
                               name(), %army name
                               name()}). %kingdom name                            
info_generic_city() ->
    record(info_generic_city, {id(),
                               id(), %player id
                               name(), %city name
                               name()}). %kingdom name

info_item_recipe() ->
    record(info_item_recipe, {id(), %type_id
                              id(), %template_id
                              id(), %player_id
                              name(), %item_name
                              name(), %flavour_text
                              materials(), %material_amount
                              materials()}). %material_type

city_form_army() ->
    record(city_form_army, {id(), %city_id,
                            string()}). %army_name

city_queue_unit() ->
    record(city_queue_unit, {id(), %city_id
                             id(), %building_id
                             unit_type(), %unit_type
                             unit_size(), %unit_size
                             caste(), %caste
                             race()}). %race

city_queue_building() ->
    record(city_queue_building, {id(), %city_id
                                 type()}). %building_type

city_craft_item() ->
    record(city_craft_item, {id(), %city_id
                              id(), %source_id
                              type(), %source_type
                              type(), %item_type
                              int()}). %amount
                             
city_queue_improvement() ->
    record(city_queue_improvement, {id(), %city_id
                                    x(), 
                                    y(), 
                                    type()}). %improvement_type

city_update_tax() ->
    record(city_update_tax, {id(), %city_id,
                             taxes()}). %taxes

add_item_recipe() ->
    record(add_item_recipe, {id(), %template_id
                             id(), %player_id,
                             string(), %item_name
                             string(), %flavour_text
                             materials()}). %material_type

add_unit_recipe() ->
    record(add_unit_recipe, {id(), %template_id,
                             id(), %player_id,
                             string(), %unit_name,
                             int(), %default_size,
                             gear()}). %gear
                                                
transfer_item() ->
    record(transfer_item, {id(),
                           source_id(),
                           source_type(),
                           target_id(),
                           target_type()}).

transfer_unit() ->
    record(transfer_unit, {unit_id(),
                           source_id(),
                           source_type(),
                           target_id(),
                           target_type()}).

battle_info() ->
    record(battle_info, {battle_id(),
                         armies(),
                         items()}).

battle_event() ->
    record(battle_event, {type(), %battle_event_type
                          battle_id(), %battle_id
                          army()}). %army
battle_target() ->
    record(battle_target, {battle_id(),
                           source_army_id(),
                           source_unit_id(),
                           target_army_id(),
                           target_unit_id()}).

battle_damage() ->
    record(battle_damage, {id(), %battle_id
                           army_unit(), %source_army_unit
                           army_unit(), %target_army_unit
                           int()}). %damage
battle_retreat() ->
    record(battle_retreat, {battle_id(),
                            source_id()}).

battle_leave() ->
    record(battle_leave, {battle_id(),
                          source_id()}).
add_claim() ->
    record(add_claim, {id(),  %city_id
                       id(),  %army_id 
                       x(),   %x
                       y()}). %y

remove_claim() ->
    record(remove_claim, {id(),
                          x(),
                          y()}).

assign_task() ->
    record(assign_task, {id(), %city_id
                         byte(), %caste
                         byte(), %race
                         amount(), %amount
                         id(), %target_id
                         type()}). %target_type                                         

remove_task() ->
    record(remove_task, {id(), %city_id
                         id()}). %assignment_id

delete_item() ->
    record(delete_item, {id()}).

create_sell_order() ->
    record(create_sell_order, {id(),
                               int()}).

create_buy_order() ->
    record(create_buy_order, {id(),
                              type(),
                              int(),
                              int()}).

fill_sell_order() ->
    record(fill_sell_order, {id(),
                             int()}).            

fill_buy_order() ->
    record(fill_buy_order, {id(),
                            int()}).                                        



%%
%% API Functions
%%

read(?CMD_POLICYREQUEST) ->
    io:fwrite("packet: CMD_POLICYREQUEST.~n"),
    policy_request;

read(<<?CMD_LOGIN, Bin/binary>>) ->
    io:fwrite("packet: read() - Read Data accepted: ~w~n", [Bin]),
    unpickle(login(), Bin);

read(<<?CMD_CLOCKSYNC>>) ->
    io:fwrite("packet: read() - clocksync~n"),
    clocksync;

read(<<?CMD_CLIENTREADY>>) ->
    io:fwrite("packet: read() - clientready~n"),
    clientready;

read(<<?CMD_CHAT_MESSAGE, Bin/binary>>) ->
    unpickle(chat_message(), Bin);

read(<<?CMD_MOVE, Bin/binary>>) ->
    unpickle(move(), Bin);

read(<<?CMD_ATTACK, Bin/binary>>) ->
    unpickle(attack(), Bin);

read(<<?CMD_ADD_WAYPOINT, Bin/binary>>) ->
    unpickle(add_waypoint(), Bin);

read(<<?CMD_REQUEST_INFO, Bin/binary>>) ->
    unpickle(request_info(), Bin);

read(<<?CMD_CITY_FORM_ARMY, Bin/binary>>) ->
    unpickle(city_form_army(), Bin);

read(<<?CMD_CITY_QUEUE_UNIT, Bin/binary>>) ->
    unpickle(city_queue_unit(), Bin);

read(<<?CMD_CITY_QUEUE_BUILDING, Bin/binary>>) ->
    unpickle(city_queue_building(), Bin);

read(<<?CMD_CITY_QUEUE_IMPROVEMENT, Bin/binary>>) ->
    unpickle(city_queue_improvement(), Bin);

read(<<?CMD_CITY_CRAFT_ITEM, Bin/binary>>) ->
    unpickle(city_craft_item(), Bin);

read(<<?CMD_CITY_UPDATE_TAX, Bin/binary>>) ->
    unpickle(city_update_tax(), Bin);

read(<<?CMD_ADD_ITEM_RECIPE, Bin/binary>>) ->
    unpickle(add_item_recipe(), Bin);

read(<<?CMD_ADD_UNIT_RECIPE, Bin/binary>>) ->
    unpickle(add_unit_recipe(), Bin);

read(<<?CMD_TRANSFER_UNIT, Bin/binary>>) ->
    unpickle(transfer_unit(), Bin);

read(<<?CMD_BATTLE_TARGET, Bin/binary>>) ->
    unpickle(battle_target(), Bin);

read(<<?CMD_BATTLE_RETREAT, Bin/binary>>) ->
    unpickle(battle_retreat(), Bin);

read(<<?CMD_BATTLE_LEAVE, Bin/binary>>) ->
    unpickle(battle_leave(), Bin);

read(<<?CMD_ADD_CLAIM, Bin/binary>>) ->
    unpickle(add_claim(), Bin);

read(<<?CMD_REMOVE_CLAIM, Bin/binary>>) ->
    unpickle(remove_claim(), Bin);

read(<<?CMD_ASSIGN_TASK, Bin/binary>>) ->
    unpickle(assign_task(), Bin);

read(<<?CMD_REMOVE_TASK, Bin/binary>>) ->
    unpickle(remove_task(), Bin);

read(<<?CMD_TRANSFER_ITEM, Bin/binary>>) ->
    unpickle(transfer_item(), Bin);

read(<<?CMD_DELETE_ITEM, Bin/binary>>) ->
    unpickle(delete_item(), Bin);

read(<<?CMD_CREATE_SELL_ORDER, Bin/binary>>) ->
    unpickle(create_sell_order(), Bin);

read(<<?CMD_CREATE_BUY_ORDER, Bin/binary>>) ->
    unpickle(create_buy_order(), Bin);

read(<<?CMD_FILL_SELL_ORDER, Bin/binary>>) ->
    unpickle(fill_sell_order(), Bin);

read(<<?CMD_FILL_BUY_ORDER, Bin/binary>>) ->
    unpickle(fill_buy_order(), Bin);

%% Test Read Packets

read(<<?CMD_PLAYER_ID, Bin/binary>>) ->
    unpickle(player_id(), Bin);

read(<<?CMD_EXPLORED_MAP, Bin/binary>>) ->
    unpickle(map(), Bin);

read(<<?CMD_PERCEPTION, Bin/binary>>) ->
    unpickle(perception(), Bin);

read(<<?CMD_INFO_ARMY, Bin/binary>>) ->
    unpickle(info_army(), Bin);

read(<<?CMD_INFO_CITY, Bin/binary>>) ->
    unpickle(info_city(), Bin);

read(<<?CMD_INFO_KINGDOM, Bin/binary>>) ->
    unpickle(info_kingdom(), Bin);

read(<<?CMD_BATTLE_INFO, Bin/binary>>) ->
    unpickle(battle_info(), Bin);

read(<<?CMD_BATTLE_EVENT, Bin/binary>>) ->
    unpickle(battle_event(), Bin);

read(<<?CMD_BATTLE_DAMAGE, Bin/binary>>) ->
    unpickle(battle_damage(), Bin);

read(<<?CMD_SUCCESS, Bin/binary>>) ->
    unpickle(success(), Bin).

write(R) when is_record(R, bad) ->
    [?CMD_BAD|pickle(bad(), R)];

write(R) when is_record(R, success) ->
    [?CMD_SUCCESS|pickle(success(), R)];

write(R) when is_record(R, chat_message) ->
    [?CMD_CHAT_MESSAGE|pickle(chat_message(), R)];

write(R) when is_record(R, player_id) ->
    [?CMD_PLAYER_ID|pickle(player_id(), R)];

write(R) when is_record(R, perception) ->
    [?CMD_PERCEPTION|pickle(perception(), R)];

write(R) when is_record(R, map) ->
    [?CMD_EXPLORED_MAP|pickle(map(), R)];

write(R) when is_record(R, info) ->
    [?CMD_INFO|pickle(info(), R)];

write(R) when is_record(R, info_kingdom) ->
    [?CMD_INFO_KINGDOM|pickle(info_kingdom(), R)];

write(R) when is_record(R, info_army) ->
    [?CMD_INFO_ARMY|pickle(info_army(), R)];

write(R) when is_record(R, info_city) ->
    [?CMD_INFO_CITY|pickle(info_city(), R)];

write(R) when is_record(R, info_tile) ->
    [?CMD_INFO_TILE|pickle(info_tile(), R)];

write(R) when is_record(R, info_generic_army) ->
    [?CMD_INFO_GENERIC_ARMY|pickle(info_generic_army(), R)];

write(R) when is_record(R, info_generic_city) ->
    [?CMD_INFO_GENERIC_CITY|pickle(info_generic_city(), R)];

write(R) when is_record(R, info_item_recipe) ->
    [?CMD_INFO_ITEM_RECIPE|pickle(info_item_recipe(), R)];

write(R) when is_record(R, battle_info) ->
    [?CMD_BATTLE_INFO|pickle(battle_info(), R)];

write(R) when is_record(R, battle_event) ->
    [?CMD_BATTLE_EVENT|pickle(battle_event(), R)];

write(R) when is_record(R, battle_damage) ->
    [?CMD_BATTLE_DAMAGE|pickle(battle_damage(), R)];

% Test write packets

write(R) when is_record(R, move) ->
    [?CMD_MOVE|pickle(move(), R)];

write(R) when is_record(R, add_waypoint) ->
    [?CMD_MOVE|pickle(add_waypoint(), R)];

write(R) when is_record(R, city_queue_improvement) ->
    [?CMD_CITY_QUEUE_IMPROVEMENT|pickle(city_queue_improvement(), R)];

write(R) when is_record(R, add_claim) ->
    [?CMD_ADD_CLAIM|pickle(add_claim(), R)];

write(R) when is_record(R, transfer_unit) ->
    [?CMD_TRANSFER_UNIT|pickle(transfer_unit(), R)];

write(R) when is_record(R, request_info) ->
    [?CMD_REQUEST_INFO|pickle(request_info(), R)];

write(R) when is_record(R, attack) ->
    [?CMD_ATTACK|pickle(attack(), R)];

write(R) when is_record(R, battle_target) ->
    [?CMD_BATTLE_TARGET|pickle(battle_target(), R)];

write(R) when is_record(R, battle_retreat) ->
    [?CMD_BATTLE_RETREAT|pickle(battle_retreat(), R)];

write(R) when is_record(R, battle_leave) ->
    [?CMD_BATTLE_LEAVE|pickle(battle_leave(), R)];

write(R) when is_record(R, city_queue_unit) ->
    [?CMD_CITY_QUEUE_UNIT|pickle(city_queue_unit(), R)];

write(R) when is_record(R, city_queue_building) ->
    [?CMD_CITY_QUEUE_BUILDING|pickle(city_queue_building(), R)];

write(R) when is_record(R, assign_task) ->
    [?CMD_ASSIGN_TASK|pickle(assign_task(), R)];

write(R) when is_record(R, transfer_item) ->
    [?CMD_TRANSFER_ITEM|pickle(transfer_item(), R)];

write(R) when is_record(R, delete_item) ->
    [?CMD_DELETE_ITEM|pickle(delete_item(), R)];

write(R) when is_record(R, create_sell_order) ->
    [?CMD_CREATE_SELL_ORDER|pickle(create_sell_order(), R)];

write(R) when is_record(R, create_buy_order) ->
    [?CMD_CREATE_BUY_ORDER|pickle(create_buy_order(), R)];

write(R) when is_record(R, fill_sell_order) ->
    [?CMD_FILL_SELL_ORDER|pickle(fill_sell_order(), R)];

write(R) when is_record(R, fill_buy_order) ->
    [?CMD_FILL_BUY_ORDER|pickle(fill_buy_order(), R)];

write(R) when is_record(R, tt) ->
    [-1|pickle(tt(), R)].

send(Socket, Data) ->
    io:format("packet: send() - Data ->  ~p~n", [Data]),
    Bin = list_to_binary(write(Data)),
    io:format("packet: send() -  ~p~n", [Bin]),
    case catch gen_tcp:send(Socket, Bin) of
        ok ->
            ok;
        {error, closed} ->
            ok;
        {error,econnaborted} ->
            ok;
        Any ->
            error_logger:error_report([
                                       {message, "gen_tcp:send error"},
                                       {module, ?MODULE}, 
                                       {line, ?LINE},
                                       {socket, Socket}, 
                                       {port_info, erlang:port_info(Socket, connected)},
                                       {data, Data},
                                       {bin, Bin},
                                       {error, Any}
                                      ])
    end.

send_policy(Socket) ->
    Bin = ?CMD_POLICY,
    io:format("packet: send() -  ~p~n", [Bin]),
    case catch gen_tcp:send(Socket, Bin) of
        ok ->
            ok,
        	gen_tcp:close(Socket);
        {error, closed} ->
            ok;
        {error,econnaborted} ->
            ok;
        Any ->
            error_logger:error_report([
                                       {message, "gen_tcp:send_policy error"},
                                       {module, ?MODULE}, 
                                       {line, ?LINE},
                                       {socket, Socket}, 
                                       {port_info, erlang:port_info(Socket, connected)},
                                       {bin, Bin},
                                       {error, Any}
                                      ])
    end.

send_clocksync(Socket) ->
    {MegaSec, Sec, MicroSec} = erlang:now(),
    io:fwrite("Mega: ~w Sec: ~w Micro: ~w", [MegaSec, Sec, MicroSec]),
    CurrentMS = (MegaSec * 1000000000) + (Sec * 1000) + (MicroSec div 1000), 
    io:fwrite("CurrentMS: ~w", [CurrentMS]),
    io:format("packet: send_clocksync() -  ~p~n", [<<?CMD_CLOCKSYNC, CurrentMS:64>>]),
    case catch gen_tcp:send(Socket, <<?CMD_CLOCKSYNC, CurrentMS:64>>) of
        ok ->
            ok;
        {error, closed} ->
            ok;
        {error,econnaborted} ->
            ok;
        Any ->
            error_logger:error_report([
                                       {message, "gen_tcp:send_clocksync error"},
                                       {module, ?MODULE}, 
                                       {line, ?LINE},
                                       {socket, Socket}, 
                                       {port_info, erlang:port_info(Socket, connected)},
                                       {error, Any}
                                      ])
    end.

%%%%
army_info_test() ->
    R = #info_army { id = 1, units = []},
    Bin = write(R),
    io:fwrite("R: ~w~n", [Bin]).

battle_joined_test() ->
    R = {battle_joined,1,[{2,1,[{1,2,3}]}]},
    Bin = write(R),
    io:fwrite("R: ~w~n", [Bin]).

tt_test() ->
    RR = #test {id = 8},
    R = #tt {test = RR},
    Bin = write(R),
    io:fwrite("R: ~w~n", [Bin]).
