%%%-------------------------------------------------------------------
%%% File    : drink_web_event_listener.erl
%%% Author  : Dan Willemsen <dan@csh.rit.edu>
%%% Purpose : 
%%%
%%%
%%% edrink, Copyright (C) 2010 Dan Willemsen
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%                         
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%-------------------------------------------------------------------

-module (drink_web_event_listener).
-behaviour (gen_server).

-include_lib ("drink/include/drink_mnesia.hrl").
-include_lib ("drink_log/include/drink_log.hrl").

-export ([start_link/1]).
-export ([init/1]).
-export ([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export ([send_msg/2]).

-record (worker, {socket, userref}).

%%%%%%%%%%%%%%%%%%%%%%%%
% Gen_Server Callbacks %
%%%%%%%%%%%%%%%%%%%%%%%%
start_link (UserRef) ->
    gen_server:start_link(?MODULE, UserRef, []).

init (UserRef) ->
    {ok, #worker{userref = UserRef, socket = nil}}.

handle_call(_, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({send_msg, Msg}, State = #worker{socket = Socket}) when Socket =/= nil ->
    % TODO: handle errors from yaws?
    yaws_api:websocket_send(Socket, Msg),
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info({dw_event, drink, _FromPid, Event}, State) ->
    Msg = encode_event(State#worker.userref, drink, Event),
    yaws_api:websocket_send(State#worker.socket, Msg),
    {noreply, State};
handle_info({ok, WebSocket}, State) ->
    % TODO: Check for already existing socket?
    dw_events:register_pid(drink, State#worker.userref),
    yaws_api:websocket_send(WebSocket, json:encode({struct, [{event, "hello"}]})),
    {noreply, State#worker{socket = WebSocket}};
handle_info({tcp, _WebSocket, DataFrame}, State) ->
    Data = yaws_websockets:unframe_all(DataFrame, []),
    [ handle_incoming_call(State, Packet) || Packet <- Data ],
    {noreply, State};
handle_info(discard, State) ->
    {stop, {shutdown, discard}, State};
handle_info({tcp_closed, WebSocket}, State = #worker{socket = WebSocket}) ->
    {stop, {shutdown, tcp_closed}, State};
handle_info({tcp_closed, _WebSocket}, State) ->
    error_logger:error_msg("Got tcp_closed for a socket we don't own!~n"),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    dw_events:unregister_pid(drink),
    user_auth:delete_ref(State#worker.userref),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

request(UserRef, Command, Args) ->
    drink_json_api:request(UserRef, Command, Args).

%%%%%%%%%%%%%%%%
% External API %
%%%%%%%%%%%%%%%%

send_msg(Ref, Msg) ->
    gen_server:cast(Ref, {send_msg, Msg}).

%%%%%%%%%%%%%%%%%%
% Internal funcs %
%%%%%%%%%%%%%%%%%%
handle_incoming_call(State, Data) ->
    case json:decode_string(binary_to_list(Data)) of
        {ok, {struct, ObjData}} ->
            case {lists:keyfind(request, 1, ObjData), lists:keyfind(id, 1, ObjData), lists:keyfind(args, 1, ObjData)} of
                {{request, Request}, {id, Id}, {args, Args}} ->
                    Result = case request(State#worker.userref, list_to_atom(Request), element(2, Args)) of
                        {ok, RespJson} ->
                            json:encode({struct, [{response, Id}, {status, "ok"}, {data, RespJson}]});
                        {error, Reason} when is_atom(Reason) ->
                            json:encode({struct, [{response, Id}, {status, "error"}, {reason, atom_to_list(Reason)}]});
                        Er ->
                            error_logger:error_msg("Got unknown result: ~p~n", [Er]),
                            json:encode({struct, [{response, Id}, {status, "error"}, {reason, "unknown"}]})
                    end,
                    yaws_api:websocket_send(State#worker.socket, Result);
                _ ->
                    error_logger:error_msg("Request not proper: ~p~n", [ObjData])
            end;
        E ->
            error_logger:error_msg("Unknown message(Error ~p): ~p~n", [E, Data])
    end.

encode_event(UserRef, drink, Event) ->
    case encode_event_data(UserRef, drink, Event) of
        false ->
            json:encode({struct, [{event, atom_to_list(element(1, Event))}]});
        Data ->
            json:encode({struct, [{event, atom_to_list(element(1, Event))},
                                  {data, Data}]})
    end.

encode_event_data(UserRef, drink, MoneyLog = #money_log{}) ->
    {struct, [{username, MoneyLog#money_log.username},
              {admin, MoneyLog#money_log.admin},
              {amount, MoneyLog#money_log.amount},
              {direction, atom_to_list(MoneyLog#money_log.direction)}]};
encode_event_data(UserRef, drink, {user_changed, Username, Changes}) ->
    {struct, [{username, Username}] ++ encode_user_changes(Changes)};
encode_event_data(UserRef, drink, {machine_added, Machine}) ->
    drink_json_api:machine_stat(user_auth:can_admin(UserRef), Machine);
encode_event_data(UserRef, drink, {machine_modified, Machine}) ->
    % TODO: we already have the full machine object, no need to get the info again
    drink_json_api:machine_stat(user_auth:can_admin(UserRef), Machine#machine.machine);
encode_event_data(UserRef, drink, {machine_deleted, Machine}) ->
    {struct, [{machineid, atom_to_list(Machine)}]};
encode_event_data(UserRef, drink, T = #temperature{}) ->
    {struct, [{machine, T#temperature.machine},
             %{time, T#temperature.time},
              {temperature, T#temperature.temperature}]};
encode_event_data(UserRef, drink, D = #drop_log{}) ->
    {struct, [{machine, D#drop_log.machine},
              {slot, D#drop_log.slot},
             %{time, D#drop_log.time},
             %{status, D#drop_log.status},
              {username, D#drop_log.username}]};
encode_event_data(UserRef, drink, _) ->
    false.

encode_user_changes([]) -> [];
encode_user_changes([{add_ibutton, IButton}|T]) ->
    [{add_ibutton, IButton}] ++ encode_user_changes(T);
encode_user_changes([{del_ibutton, IButton}|T]) ->
    [{del_ibutton, IButton}] ++ encode_user_changes(T);
encode_user_changes([{admin, Old, New}|T]) ->
    [{admin, {struct, [{old, Old}, {new, New}]}}] ++ encode_user_changes(T);
encode_user_changes([_|T]) ->
    [] ++ encode_user_changes(T).
