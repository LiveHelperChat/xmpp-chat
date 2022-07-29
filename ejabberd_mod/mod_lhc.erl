%%%----------------------------------------------------------------------
%%% File    : mod_lhc.erl
%%% Author  : Remigijus Kiminas <remdex@gmail.com>
%%% Purpose : Notyfy LHC about connected and unconnected operators
%%% Created : 3 May 2012 by Remigijus Kiminas <remdex@gmail.com>>
%%%
%%%
%%% ejabberd, Copyright (C) 2015   Live Helper Chat
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
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(mod_lhc).

-behavior(gen_mod).

-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("translate.hrl").
-include("ejabberd_http.hrl").

-export([start/2, stop/1, on_set/4, on_unset/4,on_filter_packet/1,create_message/1,create_message/3,process/2]).

-export([mod_opt_type/1, mod_options/1, depends/2]).

start(Host, _Opts) ->
   ejabberd_hooks:add(set_presence_hook, Host, ?MODULE, on_set, 50),
   ejabberd_hooks:add(unset_presence_hook, Host, ?MODULE, on_unset, 50),
   ejabberd_hooks:add(filter_packet, global, ?MODULE, on_filter_packet, 50),
   ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, create_message, 50),
   ok.

stop(Host) ->
   ejabberd_hooks:delete(set_presence_hook, Host, ?MODULE, on_set, 50),
   ejabberd_hooks:delete(unset_presence_hook, Host, ?MODULE, on_unset, 50),
   ejabberd_hooks:delete(filter_packet, global, ?MODULE, on_filter_packet, 5),
   ejabberd_hooks:delete(offline_message_hook, Host, ?MODULE, create_message, 50),
   ok.
			 
create_message(Packet) ->
    Packet.

create_message(_From, _To, _Packet) ->
   stop.
    
%% We send to LHC only chats from users in the operators shared roster group to users that start with "visitor." in all cases using web interface
on_filter_packet(#message{from = From, to = To, type = 'chat', body = Body} = Packet) when Body /= [] ->
    #jid{user = FromUser, lserver = LServer} = From,
    #jid{user = LReceiverUser, lserver = _LReceiverServer} = To,
    case mod_shared_roster:is_user_in_group({FromUser, LServer}, <<"operators">>, LServer)
         andalso re:run(LReceiverUser, "^visitor\.[0-9](.*?)") of
        'false' -> ok;
        nomatch -> ok;
        {match, _} -> 
	    %% Send only chat type with non empty body
	    %% In the future possible to extend and send typing status here just parse XML
	    if (Body /= false) -> 
			 Method = post,
			 URL = gen_mod:get_module_opt(LServer, ?MODULE, message_address),
		     BaseDomain = gen_mod:get_module_opt(LServer, ?MODULE, basedomain),
		     AHProtocol = gen_mod:get_module_opt(LServer, ?MODULE, ahprotocol),
		     AHEnviroment = gen_mod:get_module_opt(LServer, ?MODULE, ahenviroment),
		                                            		 
			 Header = [],
			 TypeMessage = "application/x-www-form-urlencoded",
             [#text{data = Data}] = Body,
			 BodyMessage = "body="++erlang:binary_to_list(misc:url_encode(Data))++
			 "&sender="++erlang:binary_to_list(misc:url_encode(FromUser))++
			 "&receiver="++erlang:binary_to_list(misc:url_encode(LReceiverUser))++
			 "&server="++erlang:binary_to_list(misc:url_encode(LServer))++
                         "&type=chat",
			 HTTPOptions = [],
			 Options = [],
			 
			 case AHEnviroment of
			    true -> 
			       [UserJID|_] = string:tokens(erlang:binary_to_list(FromUser),"@"), 
			       httpc:request(Method, {erlang:binary_to_list(AHProtocol)++ re:replace(lists:last(string:tokens(UserJID,".")),"-",".",[{return,list}]) ++ "." ++ erlang:binary_to_list(BaseDomain) ++ "/xmppservice/processmessage", Header, TypeMessage, BodyMessage}, HTTPOptions, Options);     
			    false -> 
			       httpc:request(Method, {URL, Header, TypeMessage, BodyMessage}, HTTPOptions, Options)
		     end;
		   		   			 
	    	 %% ?INFO_MSG("Need store body",[BodyMessage,FromUser]);
	    true ->
		    false
	    end
	end,
    Packet;
%% We send to LHC only groupchats from users in the operators shared roster group to users that have "operator." subscribed to the MUC in all cases using web interface
on_filter_packet(#message{from = From, to = To, type = 'groupchat', body = Body} = Packet) when Body /= [] ->
    #jid{user = FromUser, lserver = LServer} = From,
    #jid{user = LReceiverUser, lserver = LReceiverServer} = To,
    case mod_muc_admin:muc_online_rooms(LReceiverServer) /= []
         andalso lists:any(fun(X) -> re:run(X, "^.*.operator@.*") /= nomatch end, mod_muc_admin:get_subscribers(LReceiverUser, LReceiverServer))
         andalso mod_shared_roster:is_user_in_group({FromUser, LServer}, <<"operators">>, LServer) of
        'false' -> ok;
        'true' -> 
	    %% Send only chat type with non empty body
	    %% In the future possible to extend and send typing status here just parse XML
	    if (Body /= false) -> 
			 Method = post,
			 URL = gen_mod:get_module_opt(LServer, ?MODULE, message_address),
		     BaseDomain = gen_mod:get_module_opt(LServer, ?MODULE, basedomain),
		     AHProtocol = gen_mod:get_module_opt(LServer, ?MODULE, ahprotocol),
		     AHEnviroment = gen_mod:get_module_opt(LServer, ?MODULE, ahenviroment),
		                                            		 
			 Header = [],
			 TypeMessage = "application/x-www-form-urlencoded",
             [#text{data = Data}] = Body,
			 BodyMessage = "body="++erlang:binary_to_list(misc:url_encode(Data))++
			 "&sender="++erlang:binary_to_list(misc:url_encode(FromUser))++
			 "&receiver="++erlang:binary_to_list(misc:url_encode(LReceiverUser))++
			 "&server="++erlang:binary_to_list(misc:url_encode(LServer))++
                         "&type=groupchat",
			 HTTPOptions = [],
			 Options = [],
			 
			 case AHEnviroment of
			    true -> 
			       [UserJID|_] = string:tokens(erlang:binary_to_list(FromUser),"@"), 
			       httpc:request(Method, {erlang:binary_to_list(AHProtocol)++ re:replace(lists:last(string:tokens(UserJID,".")),"-",".",[{return,list}]) ++ "." ++ erlang:binary_to_list(BaseDomain) ++ "/xmppservice/processmessage", Header, TypeMessage, BodyMessage}, HTTPOptions, Options);     
			    false -> 
			       httpc:request(Method, {URL, Header, TypeMessage, BodyMessage}, HTTPOptions, Options)
		     end;
		   		   			 
	    	 %% ?INFO_MSG("Need store body",[BodyMessage,FromUser]);
	    true ->
		    false
	    end
	end,
    Packet;
%% We send to LHC only if presence is to a MUC with operator users
on_filter_packet(#presence{from = From, to = To, type = Type} = Packet) ->
    #jid{user = FromUser, lserver = LServer} = From,
    #jid{user = LReceiverUser, lserver = LReceiverServer} = To,
    Rooms = 
    case catch mod_muc_admin:muc_online_rooms(LReceiverServer) of
        R when is_list(R) -> R;
        _ -> []
    end,
    case Rooms /= []
         andalso lists:any(fun(X) -> re:run(X, "^.*.operator@.*") /= nomatch end, mod_muc_admin:get_subscribers(LReceiverUser, LReceiverServer))
         andalso mod_shared_roster:is_user_in_group({FromUser, LServer}, <<"operators">>, LServer) of
        'false' -> ok;
        'true' ->
             Method = post,
             URL = gen_mod:get_module_opt(LServer, ?MODULE, message_address),
             BaseDomain = gen_mod:get_module_opt(LServer, ?MODULE, basedomain),
             AHProtocol = gen_mod:get_module_opt(LServer, ?MODULE, ahprotocol),
             AHEnviroment = gen_mod:get_module_opt(LServer, ?MODULE, ahenviroment),

             Header = [],
             TypeMessage = "application/x-www-form-urlencoded",
             BodyMessage = "status="++atom_to_list(Type)++
             "&sender="++erlang:binary_to_list(misc:url_encode(FromUser))++
             "&receiver="++erlang:binary_to_list(misc:url_encode(LReceiverUser))++
             "&server="++erlang:binary_to_list(misc:url_encode(LServer))++
             "&type=presence",
             HTTPOptions = [],
             Options = [],

             case AHEnviroment of
                true ->
                   [UserJID|_] = string:tokens(erlang:binary_to_list(FromUser),"@"),
                   httpc:request(Method, {erlang:binary_to_list(AHProtocol)++ re:replace(lists:last(string:tokens(UserJID,".")),"-",".",[{return,list}]) ++ "." ++ erlang:binary_to_list(BaseDomain) ++ "/xmppservice/processmessage", Header, TypeMessage, BodyMessage}, HTTPOptions, Options);
                false ->
                   httpc:request(Method, {URL, Header, TypeMessage, BodyMessage}, HTTPOptions, Options)
             end
    end,
    Packet;

on_filter_packet(Packet) ->
    Packet.
    
on_set(User, Server, _Resource, _Packet) ->
   LUser = jid:nodeprep(User),
   LServer = jid:nodeprep(Server),
   %%_SID = ejabberd_sm:get_session_pid(LUser, LServer, Resource),
   
   %% Inform about new connection only if operator is connected
   case re:run(erlang:binary_to_list(LUser),"^visitor\.[0-9](.*?)") of
	  {match, _} ->  ok;
	  nomatch -> 
		   Method = post,
		   URL = gen_mod:get_module_opt(LServer, ?MODULE, login_address),
		   BaseDomain = gen_mod:get_module_opt(LServer, ?MODULE, basedomain),
		   AHProtocol = gen_mod:get_module_opt(LServer, ?MODULE, ahprotocol),
		   AHEnviroment = gen_mod:get_module_opt(LServer, ?MODULE, ahenviroment),
			
		   Header = [],
		   Type = "application/json",
		   Body = "{\"action\":\"connect\",\"user\":\""++erlang:binary_to_list(LUser)++"\",\"server\":\""++erlang:binary_to_list(LServer)++"\"}",   
		   HTTPOptions = [],
		   Options = [],
		   
		   case AHEnviroment of
			    true -> 
			       [UserJID|_] = string:tokens(erlang:binary_to_list(LUser),"@"), 
			       httpc:request(Method, {erlang:binary_to_list(AHProtocol)++ re:replace(lists:last(string:tokens(UserJID,".")),"-",".",[{return,list}]) ++ "." ++ erlang:binary_to_list(BaseDomain) ++ "/xmppservice/operatorstatus", Header, Type, Body}, HTTPOptions, Options);      
			       %% ?INFO_MSG("Automated hosting enviroment",["Automated"]);
			    false -> 
			       httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options)
		   end,
           ?INFO_MSG("Presence set demo compile Request was send ~p",[Body])
   end.
          

on_unset(User, Server, _Resource, _Packet) ->
   LUser = jid:nodeprep(User),
   LServer = jid:nodeprep(Server),
       
   %% Inform about closed connection only if operator is disconnected
   case re:run(erlang:binary_to_list(LUser),"^visitor\.[0-9](.*?)") of
	  {match, _} ->  ok;
   nomatch -> 
	   Method = post, 
	   URL = gen_mod:get_module_opt(LServer, ?MODULE, logout_address),
	   BaseDomain = gen_mod:get_module_opt(LServer, ?MODULE, basedomain),
	   AHProtocol = gen_mod:get_module_opt(LServer, ?MODULE, ahprotocol),
	   AHEnviroment = gen_mod:get_module_opt(LServer, ?MODULE, ahenviroment),
	   Header = [],
	   Type = "application/json",
	   Body = "{\"action\":\"disconnect\",\"user\":\""++erlang:binary_to_list(LUser)++"\",\"server\":\""++erlang:binary_to_list(LServer)++"\"}",   
	   HTTPOptions = [],
	   Options = [],
	   
	   case AHEnviroment of
			    true -> 
			       [UserJID|_] = string:tokens(erlang:binary_to_list(LUser),"@"), 
			       httpc:request(Method, {erlang:binary_to_list(AHProtocol)++ re:replace(lists:last(string:tokens(UserJID,".")),"-",".",[{return,list}]) ++ "." ++ erlang:binary_to_list(BaseDomain) ++ "/xmppservice/operatorstatus", Header, Type, Body}, HTTPOptions, Options);      
			       %% ?INFO_MSG("Automated hosting enviroment",["Automated"]);
			    false -> 
			       httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options)
	   end	   
   end.
    
  %% process any request to "/sockets"
	process([<<"makeonline">>], _Request) ->
		
	    % FIXME: implementation goes here
	    "Not implemented yet";
	
	%% process all remaining requests
	process(_Page, _Request) ->
	    % FIXME: implementation goes here	
	    "Fallback result".
      
    
%%   ?INFO_MSG("Presence set demo un-set ~p ~p", [erlang:binary_to_list(LUser),erlang:binary_to_list(LServer)]).


mod_opt_type(message_address) ->
    econf:string();
mod_opt_type(login_address) ->
    econf:string();
mod_opt_type(logout_address) ->
    econf:string();
mod_opt_type(ahenviroment) ->
    econf:bool();
mod_opt_type(basedomain) ->
    econf:string();
mod_opt_type(ahprotocol) ->
    econf:string().

mod_options(_Host) ->
    [{message_address, <<"http://127.0.0.1/xmppservice/operatorstatus">>},
     {login_address, <<"http://127.0.0.1/xmppservice/operatorstatus">>},
     {logout_address, <<"http//127.0.0.1/xmppservice/operatorstatus">>},
     {ahenviroment, 'false'},
     {basedomain, ""},
     {ahprotocol, "http://"}].

depends(_Host, _Opts) ->
    [].

