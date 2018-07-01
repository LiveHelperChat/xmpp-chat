%%%----------------------------------------------------------------------
%%% File    : mod_ping.erl
%%% Author  : Brian Cully <bjc@kublai.com>, Remigijus Kiminas <remdex@gmail.com>
%%% Purpose : Support XEP-0199 XMPP Ping and periodic keepalives
%%% Created : 11 Jul 2009 by Brian Cully <bjc@kublai.com>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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
%%% This is modified version of mod_ping it adds the following changes
%%% It pings back LHC instance to make sure that operator are online.
%%% @todo
%%% 1. Finish implementation :)
%%% 2. All ejabberd users starting with visitor.<number> should not be pinged only operators.
%%% 3. Settings should be loaded from config
%%%----------------------------------------------------------------------

-module(mod_lhcping).

-behaviour(gen_mod).

-behaviour(gen_server).

-include("logger.hrl").

-include("xmpp.hrl").

-define(SUPERVISOR, ejabberd_gen_mod_sup).

-define(DEFAULT_SEND_PINGS, false).

-define(DEFAULT_PING_INTERVAL, 60).

-define(DICT, dict).

%% API
-export([start_link/2, start_ping/2, stop_ping/2]).

%% gen_mod callbacks
-export([start/2, stop/1,depends/2]).

%% gen_server callbacks
-export([init/1, terminate/2, handle_call/3,
	 handle_cast/2, handle_info/2, code_change/3]).

%% Hook callbacks
-export([iq_ping/1, user_online/3, user_offline/3,
	 user_send/1, mod_opt_type/1]).

-record(state,
	{host = <<"">>,
     send_pings = ?DEFAULT_SEND_PINGS :: boolean(),
	 ping_interval = ?DEFAULT_PING_INTERVAL :: non_neg_integer(),
	 timeout_action = none :: none | kill,
	 ping_address,
	 ahenviroment,
	 basedomain,
	 ahprotocol,
     timers = (?DICT):new() :: dict:dict()}).

%%====================================================================
%% API
%%====================================================================
start_link(Host, Opts) ->
    ?INFO_MSG("start_link, LHC Ping", []),
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:start_link({local, Proc}, ?MODULE,
			  [Host, Opts], []).

start_ping(Host, JID) ->
    ?INFO_MSG("start_ping, LHC Ping", []),
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:cast(Proc, {start_ping, JID}).

stop_ping(Host, JID) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:cast(Proc, {stop_ping, JID}).

%%====================================================================
%% gen_mod callbacks
%%====================================================================
start(Host, Opts) ->
    ?INFO_MSG("start, LHC Ping", []),
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    ?INFO_MSG("start, Got Proc", []),
    PingSpec = {Proc, {?MODULE, start_link, [Host, Opts]},
        transient, 2000, worker, [?MODULE]},
    ?INFO_MSG("start, Got Ping Spec", []),       
    supervisor:start_child(?SUPERVISOR, PingSpec),
    ?INFO_MSG("start, Got SUPERVISOR", []).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:call(Proc, stop),
    supervisor:delete_child(?SUPERVISOR, Proc).
    
depends(_Host, _Opts) ->
            [].
%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Host, Opts]) ->
    ?INFO_MSG("start, init", []),
    SendPings = gen_mod:get_opt(send_pings, Opts,
                                fun(B) when is_boolean(B) -> B end,
				?DEFAULT_SEND_PINGS),
    PingInterval = gen_mod:get_opt(ping_interval, Opts,
                                   fun(I) when is_integer(I), I>0 -> I end,
				   ?DEFAULT_PING_INTERVAL),
	
    TimeoutAction = gen_mod:get_opt(timeout_action, Opts,
                                    fun(none) -> none;
                                       (kill) -> kill
                                    end, none),
    PingAddress = gen_mod:get_opt(ping_address, Opts,
                                fun iolist_to_binary/1,
                                undefined),
    BaseDomain = gen_mod:get_opt(basedomain, Opts,
                                fun iolist_to_binary/1,
                                undefined),
    AHProtocol = gen_mod:get_opt(ahprotocol, Opts,
                                fun iolist_to_binary/1,
                                undefined),
    AHEnviroment = gen_mod:get_opt(ahenviroment, Opts,
                                fun(B) when is_boolean(B) -> B end,
				false),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             no_queue),
    %%mod_disco:register_feature(Host, ?NS_PING),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_PING, ?MODULE, iq_ping, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_PING, ?MODULE, iq_ping, IQDisc),
    case SendPings of
      true ->
	  ejabberd_hooks:add(sm_register_connection_hook, Host,
			     ?MODULE, user_online, 100),
	  ejabberd_hooks:add(sm_remove_connection_hook, Host,
			     ?MODULE, user_offline, 100),
	  ejabberd_hooks:add(user_send_packet, Host, ?MODULE,
			     user_send, 100);
      _ -> ok
    end,
    {ok,
     #state{host = Host, send_pings = SendPings,
	    ping_interval = PingInterval,
	    timeout_action = TimeoutAction,
	    ping_address = PingAddress,
	    ahenviroment = AHEnviroment,
	    basedomain = BaseDomain,
	    ahprotocol = AHProtocol,
	    timers = (?DICT):new()}}.

terminate(_Reason, #state{host = Host}) ->
    ejabberd_hooks:delete(sm_remove_connection_hook, Host,
			  ?MODULE, user_offline, 100),
    ejabberd_hooks:delete(sm_register_connection_hook, Host,
			  ?MODULE, user_online, 100),
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE,
			  user_send, 100),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_PING),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     ?NS_PING).
  %%  mod_disco:unregister_feature(Host, ?NS_PING).

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Req, _From, State) ->
    {reply, {error, badarg}, State}.

handle_cast({start_ping, JID}, State) ->
    Timers = add_timer(JID, State#state.ping_interval,
		       State#state.timers),
    {noreply, State#state{timers = Timers}};
handle_cast({stop_ping, JID}, State) ->
    Timers = del_timer(JID, State#state.timers),
    {noreply, State#state{timers = Timers}};
handle_cast({iq_pong, JID, timeout}, State) ->
    Timers = del_timer(JID, State#state.timers),
    ejabberd_hooks:run(user_ping_timeout, State#state.host,
		       [JID]),
    case State#state.timeout_action of
      kill ->
	  #jid{user = User, server = Server,
	       resource = Resource} =
	      JID,
	  case ejabberd_sm:get_session_pid(User, Server, Resource)
	      of
	    Pid when is_pid(Pid) -> ejabberd_c2s:stop(Pid);
	    _ -> ok
	  end;
      _ -> ok
    end,
    {noreply, State#state{timers = Timers}};
handle_cast(_Msg, State) -> {noreply, State}.

handle_info({timeout, _TRef, {ping, JID}}, State) ->
%%    IQ = #iq{type = get,
%%     sub_els =
%%		 [#xmlel{name = <<"ping">>,
%%			 attrs = [{<<"xmlns">>, ?NS_PING}], children = []}]},
    Host = State#state.host,
%%    Pid = self(),
%%    F = fun (Response) ->
%%		gen_server:cast(Pid, {iq_pong, JID, Response})
%%	end,
	
	%% @todo by jid determine instance address for automated hosting support		
	Method = post,  
    Header = [],
    Type = "application/json",
    Body = "{\"action\":\"ping\",\"user\":\""++erlang:binary_to_list(jlib:jid_to_string(JID))++"\"}",   
    HTTPOptions = [],
    Options = [],
      
    case State#state.ahenviroment of
	    true -> 
	       [UserJID|_] = string:tokens(erlang:binary_to_list(jlib:jid_to_string(JID)),"@"), 
	       httpc:request(Method, {erlang:binary_to_list(State#state.ahprotocol)++ re:replace(lists:last(string:tokens(UserJID,".")),"-",".",[{return,list}]) ++ "." ++ erlang:binary_to_list(State#state.basedomain) ++ "/xmppservice/operatorstatus", Header, Type, Body}, HTTPOptions, Options);       
	       %% ?INFO_MSG("Automated hosting enviroment",[Subdomain]);
	    false -> 
	    httpc:request(Method, {State#state.ping_address, Header, Type, Body}, HTTPOptions, Options)
	end,
     		
    From = jlib:make_jid(<<"">>, State#state.host, <<"">>),
    IQ = #iq{from = From, to = JID, type = get, sub_els = [#ping{}]},

    ejabberd_router:route_iq(IQ, JID, gen_mod:get_module_proc(Host, ?MODULE), State#state.ping_interval),
    Timers = add_timer(JID, State#state.ping_interval,
		       State#state.timers),
    {noreply, State#state{timers = Timers}};
handle_info(_Info, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%====================================================================
%% Hook callbacks
%%====================================================================
iq_ping(#iq{type = Type, sub_els = SubEl} = IQ) ->

    case {Type, SubEl} of
      {get, #xmlel{name = <<"ping">>}} ->
	  IQ#iq{type = result, sub_els = []};
      _ ->
	  IQ#iq{type = error,
		sub_els = [SubEl, xmpp:err_feature_not_implemented()]}
    end.

%% Activate pinging only to operators
user_online(_SID, JID, _Info) ->	
	case re:run(jlib:jid_to_string(JID),"^visitor\.[0-9](.*?)") of
	  {match, _} -> ok;
	  nomatch -> start_ping(JID#jid.lserver, JID)
	end.

user_offline(_SID, JID, _Info) ->
    stop_ping(JID#jid.lserver, JID).

%% Activate pinging only to operators
user_send({Packet, C2SState}) ->
    From = xmpp:get_from(Packet),
%%    To = xmpp:get_to(Packet),	
    case re:run(jlib:jid_to_string(From),"^visitor\.[0-9](.*?)") of
	  {match, _} ->  ok;
	  nomatch -> start_ping(From#jid.lserver, From)
	end,
	{Packet, C2SState}.

%%====================================================================
%% Internal functions
%%====================================================================
add_timer(JID, Interval, Timers) ->
    LJID = jlib:jid_tolower(JID),
    NewTimers = case (?DICT):find(LJID, Timers) of
		  {ok, OldTRef} ->
		      cancel_timer(OldTRef), (?DICT):erase(LJID, Timers);
		  _ -> Timers
		end,
    TRef = erlang:start_timer(Interval * 1000, self(),
			      {ping, JID}),
    (?DICT):store(LJID, TRef, NewTimers).

del_timer(JID, Timers) ->
    LJID = jlib:jid_tolower(JID),
    case (?DICT):find(LJID, Timers) of
      {ok, TRef} ->
	  cancel_timer(TRef), (?DICT):erase(LJID, Timers);
      _ -> Timers
    end.

cancel_timer(TRef) ->
    case erlang:cancel_timer(TRef) of
      false ->
	  receive {timeout, TRef, _} -> ok after 0 -> ok end;
      _ -> ok
    end.
    
mod_opt_type(iqdisc) -> fun gen_iq_handler:check_type/1;
mod_opt_type(ping_interval) ->
    fun (I) when is_integer(I), I > 0 -> I end;
mod_opt_type(send_pings) ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(timeout_action) ->
    fun (none) -> none;
	(kill) -> kill
    end;
mod_opt_type(ping_address) ->
    fun(Val) when is_binary(Val) -> binary_to_list(Val);
    (Val) -> Val
    end;
mod_opt_type(ahenviroment) ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(basedomain) ->
    fun(Val) when is_binary(Val) -> binary_to_list(Val);
    (Val) -> Val
    end;
mod_opt_type(ahprotocol) ->
    fun(Val) when is_binary(Val) -> binary_to_list(Val);
    (Val) -> Val
    end;           
mod_opt_type(_) ->
    [iqdisc, ping_interval, send_pings, timeout_action, ping_address, ahenviroment, basedomain, ahprotocol].