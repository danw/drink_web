%%%-------------------------------------------------------------------
%%% File    : ssl_redirect.erl
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

-module (ssl_redirect).

-export ([out/1]).

-include_lib ("yaws/include/yaws_api.hrl").

out(Arg) ->
    Headers = Arg#arg.headers,
    Query = case Arg#arg.querydata of
        [] ->
            [];
        Data when is_list(Data) ->
            "?" ++ Data;
        _ ->
            []
    end,
    case Headers#headers.host of
        undefined ->
            {redirect, "https://erlang" ++ Arg#arg.server_path ++ Query};
        Host ->
            {redirect, "https://" ++ hd(string:tokens(Host, ":")) ++ Arg#arg.server_path ++ Query}
    end.
