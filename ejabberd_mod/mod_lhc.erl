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
-include("ejabberd.hrl").
-include("logger.hrl").
-include("ejabberd.hrl").
-include("ejabberd_http.hrl").
-include("jlib.hrl").

-export([start/2, stop/1, on_set/4, on_unset/4,on_filter_packet/1]).

start(Host, _Opts) ->
   ejabberd_hooks:add(set_presence_hook, Host, ?MODULE, on_set, 50),
   ejabberd_hooks:add(unset_presence_hook, Host, ?MODULE, on_unset, 50),
   ejabberd_hooks:add(filter_packet, global, ?MODULE, on_filter_packet, 50),
   ok.

stop(Host) ->
   ejabberd_hooks:delete(set_presence_hook, Host, ?MODULE, on_set, 50),
   ejabberd_hooks:delete(unset_presence_hook, Host, ?MODULE, on_unset, 50),
   ejabberd_hooks:delete(filter_packet, global, ?MODULE, on_filter_packet, 50),
   ok.
   
on_filter_packet({From, To, XML} = Packet) ->
        
    #jid{user = LUser, lserver = LServer} = From,
    
    %% We send to LHC only request from operators, like visitors in all cases are using web interface
    case re:run(LUser,"^visitor\.[0-9](.*?)") of
	  {match, _} -> ok;
	  nomatch -> 
	  	Type = xml:get_tag_attr_s(<<"type">>, XML),
	    Body = xml:get_subtag(XML, <<"body">>),
	    %% Send only chat type with non empty body
	    %% In the future possible to extend and send typing status here just parse XML
	    if (Type == <<"chat">>) and (Body /= false) -> 
	    	 #jid{user = LReceiverUser, lserver = _LReceiverServer} = To,
			 Method = post,
			 URL = gen_mod:get_module_opt(LServer, ?MODULE, message_address,
		                                            fun iolist_to_binary/1,
		                                            undefined),			 
			 Header = [],
			 TypeMessage = "application/x-www-form-urlencoded",
			 BodyMessage = "body="++erlang:binary_to_list(ejabberd_http:url_encode(xml:get_tag_cdata(Body)))++
			 "&sender="++erlang:binary_to_list(ejabberd_http:url_encode(LUser))++
			 "&receiver="++erlang:binary_to_list(ejabberd_http:url_encode(LReceiverUser))++
			 "&server="++erlang:binary_to_list(ejabberd_http:url_encode(LServer)),
			 HTTPOptions = [],
			 Options = [],
			 httpc:request(Method, {erlang:binary_to_list(URL), Header, TypeMessage, BodyMessage}, HTTPOptions, Options);
	    	 %% ?INFO_MSG("Need store body",[BodyMessage,LUser]);
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
		   Header = [],
		   Type = "application/json",
		   Body = "{\"action\":\"connect\",\"user\":\""++erlang:binary_to_list(LUser)++"\",\"server\":\""++erlang:binary_to_list(LServer)++"\"}",   
		   HTTPOptions = [],
		   Options = [],
		   httpc:request(Method, {erlang:binary_to_list(URL), Header, Type, Body}, HTTPOptions, Options)
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
	   Header = [],
	   Type = "application/json",
	   Body = "{\"action\":\"disconnect\",\"user\":\""++erlang:binary_to_list(LUser)++"\",\"server\":\""++erlang:binary_to_list(LServer)++"\"}",   
	   HTTPOptions = [],
	   Options = [],
	   httpc:request(Method, {erlang:binary_to_list(URL), Header, Type, Body}, HTTPOptions, Options)
   end.
    
   %%?INFO_MSG("Presence set demo un-set %p %p", [erlang:binary_to_list(LUser),erlang:binary_to_list(LServer)]).