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

-behaviour(gen_mod).
-include("logger.hrl").
-include("ejabberd_http.hrl").
-include("xmpp.hrl").

-export([start/2, stop/1, on_set/4, on_unset/4,on_filter_packet/1,create_message/1,process/2,depends/2]).

start(Host, _Opts) ->
   ejabberd_hooks:add(set_presence_hook, Host, ?MODULE, on_set, 50),
   ejabberd_hooks:add(unset_presence_hook, Host, ?MODULE, on_unset, 50),
   ejabberd_hooks:add(filter_packet, global, ?MODULE, on_filter_packet, 50),
   ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, create_message, 50),
   ok.

stop(Host) ->
   ejabberd_hooks:delete(set_presence_hook, Host, ?MODULE, on_set, 50),
   ejabberd_hooks:delete(unset_presence_hook, Host, ?MODULE, on_unset, 50),
   ejabberd_hooks:delete(filter_packet, global, ?MODULE, on_filter_packet, 50),
   ejabberd_hooks:delete(offline_message_hook, Host, ?MODULE, create_message, 50),
   ok.
depends(_Host, _Opts) ->
            [].			 
create_message({_Action, Acc}) ->
   stop.
    
on_filter_packet(Packet) ->

	From = xmpp:get_from(Packet),
	To =  xmpp:get_to(Packet),    
	#jid{user = LUser, lserver = LServer} = From,
    
	?INFO_MSG("This is from ~p and this is to ~p", [From,To]),

    %% We send to LHC only request from operators, like visitors in all cases are using web interface
    case re:run(LUser,"^visitor\.[0-9](.*?)") of
	  {match, _} -> ok;
	  nomatch -> 
		%%Type = xml:get_tag_attr_s(<<"type">>, XML),
		%%Body = xml:get_subtag(XML, <<"body">>),

		Type = xmpp:get_type(Packet),  
		Msg = xmpp:get_name(Packet) ,
		?INFO_MSG("This is type ~p", [Type]),
		?INFO_MSG("This is Operator and packet name ~p", [Msg]),

		Body = if 
			(Msg == <<"message">>) -> xmpp:get_text(Packet#message.body);			
		true ->	
				false
		end,

		?INFO_MSG("This is body ~p and this is stanza type ~p", [Body,Msg]),

	    %% Send only chat type with non empty body
	    %% In the future possible to extend and send typing status here just parse XML
	    if (Type == chat) and (Body /= false) -> 
	    	 #jid{user = LReceiverUser, lserver = _LReceiverServer} = To,
			 Method = post,
			 URL = gen_mod:get_module_opt(LServer, ?MODULE, message_address,
		                                            fun iolist_to_binary/1,
		                                            undefined),	
		                                            
		     BaseDomain = gen_mod:get_module_opt(LServer, ?MODULE, basedomain,
		                                            fun iolist_to_binary/1,
		                                            undefined),
		                                            
		     AHProtocol = gen_mod:get_module_opt(LServer, ?MODULE, ahprotocol,
		                                            fun iolist_to_binary/1,
		                                            undefined),
		   
		     AHEnviroment = gen_mod:get_module_opt(LServer, ?MODULE, ahenviroment,
                                fun(B) when is_boolean(B) -> B end, 
                                false),
		                                            
		                                            		 
			 Header = [],
			 TypeMessage = "application/x-www-form-urlencoded",
			 BodyMessage = "body="++erlang:binary_to_list(misc:url_encode(Body))++
			 "&sender="++erlang:binary_to_list(misc:url_encode(LUser))++
			 "&receiver="++erlang:binary_to_list(misc:url_encode(LReceiverUser))++
			 "&server="++erlang:binary_to_list(misc:url_encode(LServer)),
			 ?INFO_MSG("Need store body ~p and ~p",[BodyMessage,LUser]),
			 HTTPOptions = [],
			 Options = [],
			 
			 case AHEnviroment of
			    true -> 
			       [UserJID|_] = string:tokens(erlang:binary_to_list(LUser),"@"), 
			       httpc:request(Method, {erlang:binary_to_list(AHProtocol)++ re:replace(lists:last(string:tokens(UserJID,".")),"-",".",[{return,list}]) ++ "." ++ erlang:binary_to_list(BaseDomain) ++ "/xmppservice/processmessage", Header, TypeMessage, BodyMessage}, HTTPOptions, Options);     
			    false -> 
			       httpc:request(Method, {erlang:binary_to_list(URL), Header, TypeMessage, BodyMessage}, HTTPOptions, Options)
		     end;
		   		   			 
	    true ->
		    false
	    end
	end,
        
    Packet.
    
on_set(User, Server, _Resource, _Packet) ->
   LUser = jlib:nodeprep(User),
   LServer = jlib:nodeprep(Server),
   %%_SID = ejabberd_sm:get_session_pid(LUser, LServer, Resource),
   
   %% Inform about new connection only if operator is connected
   case re:run(erlang:binary_to_list(LUser),"^visitor\.[0-9](.*?)") of
	  {match, _} ->  ok;
	  nomatch -> 
		   Method = post,
		   URL = gen_mod:get_module_opt(LServer, ?MODULE, login_address,
		                                            fun iolist_to_binary/1,
		                                            undefined),
		                                            
		   BaseDomain = gen_mod:get_module_opt(LServer, ?MODULE, basedomain,
		                                            fun iolist_to_binary/1,
		                                            undefined),
		                                            
		   AHProtocol = gen_mod:get_module_opt(LServer, ?MODULE, ahprotocol,
		                                            fun iolist_to_binary/1,
		                                            undefined),
		   
		   AHEnviroment = gen_mod:get_module_opt(LServer, ?MODULE, ahenviroment,
                                fun(B) when is_boolean(B) -> B end, 
                                false),
			
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
			       httpc:request(Method, {erlang:binary_to_list(URL), Header, Type, Body}, HTTPOptions, Options)
		   end
		  
   end.
          
   %%?INFO_MSG("Presence set demo compile Request was send %p",[Body]).

on_unset(User, Server, _Resource, _Packet) ->
   LUser = jlib:nodeprep(User),
   LServer = jlib:nodeprep(Server),
       
   %% Inform about closed connection only if operator is disconnected
   case re:run(erlang:binary_to_list(LUser),"^visitor\.[0-9](.*?)") of
	  {match, _} ->  ok;
   nomatch -> 
	   Method = post, 
	   URL = gen_mod:get_module_opt(LServer, ?MODULE, logout_address,
	                                            fun iolist_to_binary/1,
	                                            undefined),
	                                            
	   BaseDomain = gen_mod:get_module_opt(LServer, ?MODULE, basedomain,
		                                            fun iolist_to_binary/1,
		                                            undefined),
		                                            
	   AHProtocol = gen_mod:get_module_opt(LServer, ?MODULE, ahprotocol,
		                                            fun iolist_to_binary/1,
		                                            undefined),
		   
	   AHEnviroment = gen_mod:get_module_opt(LServer, ?MODULE, ahenviroment,
                                fun(B) when is_boolean(B) -> B end, 
                                false),
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
			       httpc:request(Method, {erlang:binary_to_list(URL), Header, Type, Body}, HTTPOptions, Options)
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
      
    
   %%?INFO_MSG("Presence set demo un-set %p %p", [erlang:binary_to_list(LUser),erlang:binary_to_list(LServer)]).