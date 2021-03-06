%%%-------------------------------------------------------------------
%%% File    : drink_web.erl
%%% Author  : Dan Willemsen <dan@csh.rit.edu>
%%% Purpose : 
%%%
%%%
%%% edrink, Copyright (C) 2008 Dan Willemsen
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

-module (drink_web).

-export ([out/1]).

-include_lib ("yaws/include/yaws_api.hrl").
-include_lib ("drink/include/user.hrl").
-include_lib ("drink/include/drink_mnesia.hrl").

out(A) ->
    User = case lists:keymember(webauth_keytab, 1, A#arg.opaque) of
        true ->
            case authmod_webauth:auth(A, undefined) of
                {true, {WebAuthUser, _, _}} -> {ok, WebAuthUser, []};
                _ -> error
            end;
        false ->
            case yaws_api:find_cookie_val("drink_user", (A#arg.headers)#headers.cookie) of
                [] ->
                    case {A#arg.appmoddata, yaws_api:queryvar(A, "user")} of
                        {"setusercookie", {ok, CookieUser}} ->
                            Cookie = yaws_api:setcookie("drink_user", CookieUser, "/"),
                            {ok, CookieUser, [Cookie]};
                        _ -> error
                    end;
                CookieUser -> {ok, CookieUser, []}
            end
    end,
    case User of
        {ok, UserName, Headers} ->
            case user_auth:auth({webauth, UserName}) of
                {ok, UserRef} ->
                    case request(A, UserRef, (A#arg.req)#http_request.method, list_to_atom(A#arg.appmoddata)) of
                        {websocket, OwnerPid, SocketMode} ->
                            {websocket, OwnerPid, SocketMode};
                        Ret ->
                            user_auth:delete_ref(UserRef),
                            Headers ++ Ret
                    end;
                _ -> Headers ++ error(login_failed)
            end;
        _ -> error(login_failed)
    end.

api(UserRef, Command, Args) ->
    case drink_json_api:request(UserRef, Command, Args) of
        {ok, Data} ->
            ok(Data);
        {error, Reason} ->
            error(Reason)
    end.

request(_, U, 'GET', currentuser) ->
    api(U, currentuser, nil);
request(_, _, _, currentuser) -> error(wrong_method);

request(A, U, 'POST', drop) ->
    case {yaws_api:postvar(A, "machine"), yaws_api:postvar(A, "slot"), yaws_api:postvar(A, "delay")} of
        {{ok, Machine}, {ok, SlotNum}, {ok, Delay}} ->
            api(U, drop, [{machine, Machine}, {slot, SlotNum}, {delay, Delay}]);
        _ ->
            error(invalid_args)
    end;
request(_, _, _, drop) -> error(wrong_method);

request(_, U, 'GET', events) ->
    {ok, Pid} = drink_web_event_listener:start_link(U),
    {websocket, Pid, true};
request(_, _, _, events) -> error(wrong_method);

request(A, U, 'GET', logs) ->
    case {yaws_api:queryvar(A, "offset"), yaws_api:queryvar(A, "limit")} of
        {{ok, Offset}, {ok, Limit}} ->
            api(U, logs, [{offset, Offset}, {limit, Limit}]);
        _ ->
            error(invalid_args)
    end;
request(_, _, _, logs) -> error(wrong_method);

request(_, U, 'GET', machines) ->
    api(U, machines, nil);
request(_, _, _, machines) -> error(wrong_method);

request(A, U, 'POST', moduser) ->
    case {yaws_api:postvar(A, "username"),
          yaws_api:postvar(A, "attr"),
          yaws_api:postvar(A, "value"),
          yaws_api:postvar(A, "reason")} of
        {{ok, UserName}, {ok, Attr}, {ok, Value}, {ok, ModReason}} ->
            api(U, moduser, [{username, UserName}, {attr, Attr}, {value, Value}, {reason, ModReason}]);
        _ ->
            error(invalid_args)
    end;
request(_, _, _, moduser) -> error(wrong_method);

request(A, U, 'POST', addslot) ->
    case {yaws_api:postvar(A, "machine"),
          yaws_api:postvar(A, "slot"),
          yaws_api:postvar(A, "name"),
          yaws_api:postvar(A, "price"),
          yaws_api:postvar(A, "available"),
          yaws_api:postvar(A, "disabled")} of
        {{ok, Machine}, {ok, Slot}, {ok, Name}, {ok, Price}, {ok, Avail}, {ok, Disabled}} ->
            api(U, addslot, [{machine, Machine}, {slot, Slot}, {name, Name}, {price, Price}, {available, Avail}, {disabled, Disabled}]);
        _ ->
            error(invalid_args)
    end;
request(_, _, _, addslot) -> error(wrong_method);

request(A, U, 'POST', setslot) ->
    case {yaws_api:postvar(A, "machine"), 
          yaws_api:postvar(A, "slot"),
          yaws_api:postvar(A, "name"),
          yaws_api:postvar(A, "price"),
          yaws_api:postvar(A, "available"),
          yaws_api:postvar(A, "disabled")} of
        {{ok, Machine}, {ok, Slot}, {ok, Name}, {ok, Price}, {ok, Avail}, {ok, Disabled}} ->
            api(U, setslot, [{machine, Machine}, {slot, Slot}, {name, Name}, {price, Price}, {available, Avail}, {disabled, Disabled}]);
        _ ->
            error(invalid_args)
    end;
request(_, _, _, setslot) -> error(wrong_method);

request(A, U, 'POST', delslot) ->
    case {yaws_api:postvar(A, "machine"),
          yaws_api:postvar(A, "slot")} of
        {{ok, Machine}, {ok, Slot}} ->
            api(U, delslot, [{machine, Machine}, {slot, Slot}]);
        _ ->
            error(invalid_args)
    end;
request(_, _, _, delslot) -> error(wrong_method);

request(A, U, 'GET', temperatures) ->
    case {yaws_api:queryvar(A, "from"), yaws_api:queryvar(A, "length")} of
        {{ok, From}, {ok, Length}} ->
            api(U, temperatures, [{from, From}, {length, Length}]);
        _ ->
            error(invalid_args)
    end;
request(_, _, _, temperatures) -> error(wrong_method);

request(A, U, 'GET', userinfo) ->
    case yaws_api:queryvar(A, "user") of
        {ok, UserName} ->
            api(U, userinfo, [{user, UserName}]);
        _Else ->
            error(invalid_args)
    end;
request(_, _, _, userinfo) -> error(wrong_method);

request(A, U, 'POST', addmachine) ->
    case {yaws_api:postvar(A, "machine"),
          yaws_api:postvar(A, "name"),
          yaws_api:postvar(A, "password"),
          yaws_api:postvar(A, "public_ip"),
          yaws_api:postvar(A, "available_sensor"),
          yaws_api:postvar(A, "machine_ip"),
          yaws_api:postvar(A, "allow_connect"),
          yaws_api:postvar(A, "admin_only")} of
        {{ok, Atom},
         {ok, Name},
         {ok, Password},
         {ok, PublicIP},
         {ok, AvailableSensor},
         {ok, MachineIP},
         {ok, AllowConnect},
         {ok, AdminOnly}} ->
             api(U, addmachine, [{machine, Atom}, {name, Name}, {password, Password}, {public_ip, PublicIP}, 
                {available_sensor, AvailableSensor}, {machine_ip, MachineIP}, {allow_connect, AllowConnect},
                {admin_only, AdminOnly}]);
        _ -> error(invalid_args)
    end;
request(_, _, _, addmachine) -> error(wrong_method);

request(A, U, 'POST', modmachine) ->
    case {yaws_api:postvar(A, "machine"),
          yaws_api:postvar(A, "name"),
          yaws_api:postvar(A, "password"),
          yaws_api:postvar(A, "public_ip"),
          yaws_api:postvar(A, "available_sensor"),
          yaws_api:postvar(A, "machine_ip"),
          yaws_api:postvar(A, "allow_connect"),
          yaws_api:postvar(A, "admin_only")} of
        {{ok, Atom},
         {ok, Name},
         {ok, Password},
         {ok, PublicIP},
         {ok, AvailableSensor},
         {ok, MachineIP},
         {ok, AllowConnect},
         {ok, AdminOnly}} ->
            api(U, modmachine, [{machine, Atom}, {name, Name}, {password, Password}, {public_ip, PublicIP},
                {available_sensor, AvailableSensor}, {machine_ip, MachineIP}, {allow_connect, AllowConnect},
                {admin_only, AdminOnly}]);
        _ -> error(invalid_args)
    end;
request(_, _, _, modmachine) -> error(wrong_method);

request(A, U, 'POST', delmachine) ->
    case yaws_api:postvar(A, "machine") of
        {ok, Machine} ->
            api(U, delmachine, [{machine, Machine}]);
        _ -> error(invalid_args)
    end;
request(_, _, _, delmachine) -> error(wrong_method);

request(A, U, 'GET', getconnections) ->
    api(U, getconnections, []);
request(_, _, _, getconnections) -> error(wrong_method);

request(A, U, 'POST', addapp) ->
    case {yaws_api:queryvar(A, "name"),
          yaws_api:queryvar(A, "description")} of
        {{ok, Name}, {ok, Description}} ->
            api(U, addapp, [{name, Name}, {description, Description}]);
        _ -> error(invalid_args)
    end;
request(_, _, _, addapp) -> error(wrong_method);

request(A, U, 'GET', getapps) ->
    api(U, getapps, []);
request(_, _, _, getapps) -> error(wrong_method);

request(A, U, 'POST', delapp) ->
    case yaws_api:queryvar(A, "name") of
        {ok, Name} ->
            api(U, delapp, [{name, Name}]);
        _ -> error(invalid_args)
    end;
request(_, _, _, delapp) -> error(wrong_method);

request(_, _, 'GET', _) ->
    error(unknown_path);
request(_, _, 'POST', _) ->
    error(unknown_path);
request(_, _, _, _) ->
    error(wrong_method).

ok(Data) ->
    [{content, "application/json", json:encode({struct, [{status, "ok"}, {data, Data}]})}].

error(Reason) when is_atom(Reason) ->
    error(atom_to_list(Reason));
error(Reason) ->
    [{content, "application/json", json:encode({struct, [{status, "error"}, {reason, Reason}]})}].

