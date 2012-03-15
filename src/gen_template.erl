%% -------------------------------------------------------------------
%% Author  : Peter
%%% Description :
%%%
%%% Created : Sept, 2011
%%% -------------------------------------------------------------------
-module(chat).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("schema.hrl").
-include("packet.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([add/3, broadcast/3]).

-record(data, {members = []}).
-record(member, {player_id,
                 player_name,
                 socket}).
%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, chat_pid}, chat, [], []).

add(PlayerId, PlayerName, Socket) ->
    gen_server:cast({global, chat_pid}, {'ADD', PlayerId, PlayerName, Socket}).

broadcast(PlayerId, PlayerName, Message) ->
    gen_server:cast({global, chat_pid}, {'BROADCAST', PlayerId, PlayerName, Message}).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    Data = #data {},
    {ok, Data}.

handle_cast('TEST', Data) ->   
    {noreply, Data};

handle_cast({'ADD', PlayerId, PlayerName, Socket}, Data) ->
    Member = #member {player_id = PlayerId,
                      player_name = PlayerName,
                      socket = Socket},

    NewMembers = [Member | Data#data.members],
    NewData = Data#data {members = NewMembers},

    {noreply, NewData};

handle_cast({'REMOVE', PlayerId}, Data) ->
    NewData = lists:keydelete(PlayerId, 1, Data#data.members), 
    {noreply, NewData};

handle_cast({'BROADCAST', PlayerId, PlayerName, Message}, Data) ->
    F = fun(Member) ->
            ChatMessage = #chat_message {player_id = PlayerId,
                                         player_name = PlayerName,
                                         message = Message},
            ?INFO("Socket", Member#member.socket),
            ?INFO("ChatMessage", ChatMessage),
            forward_to_client(Member#member.socket, ChatMessage)
        end,
    
    ?INFO("Members", Data#data.members),
    lists:foreach(F, Data#data.members),
    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({'GET_ARMIES', PlayerId}, _From, Data) ->
    [Kingdom] = db:dirty_index_read(kingdom, PlayerId, #kingdom.player_id), 
    {reply, Kingdom#kingdom.armies, Data};

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

terminate(_Reason, _) ->
    ok.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

forward_to_client(Socket, Event) ->
    if
        Socket /= none ->
            Socket ! {packet, Event};
        true ->
            ok
    end.
