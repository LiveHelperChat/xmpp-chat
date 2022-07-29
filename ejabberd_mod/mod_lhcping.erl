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

-author('bjc@kublai.com').

-behavior(gen_mod).

-behavior(gen_server).

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").
-include("translate.hrl").

-define(SUPERVISOR, ejabberd_sup).

-define(DEFAULT_SEND_PINGS, false).

-define(DEFAULT_PING_INTERVAL, 60).

-define(DICT, dict).

%% API
-export([start_ping/2, stop_ping/2]).

%% gen_mod callbacks
-export([start/2, stop/1]).

%% gen_server callbacks
-export([init/1, terminate/2, handle_call/3,
	 handle_cast/2, handle_info/2, code_change/3]).

%% Hook callbacks
-export([iq_ping/1, user_online/3, user_offline/3]).

-export([mod_opt_type/1, mod_options/1, depends/2, mod_doc/0]). 

-record(state,
	{host = <<"">>,
         send_pings = ?DEFAULT_SEND_PINGS :: boolean(),
	 ping_interval = ?DEFAULT_PING_INTERVAL :: non_neg_integer(),
	 timeout_action = none :: none | kill,
	 ping_address,
	 ahenviroment,
	 basedomain,
	 ahprotocol,
         timers = (?DICT):new()}).

%%====================================================================
%% API
%%====================================================================
start_ping(Host, ID) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:cast(Proc, {start_ping, ID}).

stop_ping(Host, ID) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:cast(Proc, {stop_ping, ID}).

%%====================================================================
%% gen_mod callbacks
%%====================================================================
start(Host, Opts) ->
    gen_mod:start_child(?MODULE, Host, Opts).

stop(Host) ->
    gen_mod:stop_child(?MODULE, Host).

reload(Host, NewOpts, OldOpts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:cast(Proc, {reload, Host, NewOpts, OldOpts}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Host|_]) ->
    process_flag(trap_exit, true),
    Opts = gen_mod:get_module_opts(Host, ?MODULE),
    State = init_state(Host, Opts),
    register_iq_handlers(Host),
    case State#state.send_pings of
        true -> register_hooks(Host);
        false -> ok
    end,
    {ok, State}.

terminate(_Reason, #state{host = Host}) ->
    unregister_hooks(Host),
    unregister_iq_handlers(Host).

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Req, _From, State) ->
    {reply, {error, badarg}, State}.

handle_cast({start_ping, ID}, State) ->
    Timers = add_timer(ID, State#state.ping_interval,
		       State#state.timers),
    {noreply, State#state{timers = Timers}};
handle_cast({stop_ping, ID}, State) ->
    Timers = del_timer(ID, State#state.timers),
    {noreply, State#state{timers = Timers}};
handle_cast({iq_pong, {_SID, JID} = ID, timeout}, State) ->
    Timers = del_timer(ID, State#state.timers),
    ejabberd_hooks:run(user_ping_timeout, State#state.host,
		       [JID]),
    case State#state.timeout_action of
      kill ->
	  #jid{user = User, server = Server,
	       resource = Resource} =
	      JID,
	  case ejabberd_sm:get_session_pid(User, Server, Resource)
	      of
	    Pid when is_pid(Pid) -> ejabberd_c2s:stop_async(Pid);
	    _ -> ok
	  end;
      _ -> ok
    end,
    {noreply, State#state{timers = Timers}};
handle_cast(_Msg, State) -> {noreply, State}.

handle_info({timeout, _TRef, {ping, {_SID, JID} = ID}}, State) ->
    From = jid:make(<<"">>, State#state.host, <<"">>),
    IQ = #iq{from = From, to = JID, type = get, sub_els = [#ping{}]},
    Pid = self(),
    F = fun (Response) ->
		gen_server:cast(Pid, {iq_pong, ID, Response})
	end,
	
	%% @todo by jid determine instance address for automated hosting support		
	Method = post,  
    Header = [],
    Type = "application/json",
    Body = "{\"action\":\"ping\",\"user\":\""++erlang:binary_to_list(jid:to_string(JID))++"\"}",   
    HTTPOptions = [],
    Options = [],
      
    case State#state.ahenviroment of
	    true -> 
	       [UserJID|_] = string:tokens(erlang:binary_to_list(jid:to_string(JID)),"@"), 
	       httpc:request(Method, {erlang:binary_to_list(State#state.ahprotocol)++ re:replace(lists:last(string:tokens(UserJID,".")),"-",".",[{return,list}]) ++ "." ++ erlang:binary_to_list(State#state.basedomain) ++ "/xmppservice/operatorstatus", Header, Type, Body}, HTTPOptions, Options);       
	       %% ?INFO_MSG("Automated hosting enviroment",[Subdomain]);
	    false -> 
	    httpc:request(Method, {State#state.ping_address, Header, Type, Body}, HTTPOptions, Options)
	end,
     		
    ?INFO_MSG("Request sent ~p",[Body]),

    ejabberd_router:route_iq(IQ, F),
    #jid{user = User, lserver = LServer} = JID,
    Timers = 
    case mod_shared_roster:is_user_in_group({User, LServer}, <<"operators">>, LServer) of
        'false' -> State#state.timers;
        'true' ->
            add_timer(ID, State#state.ping_interval,
                      State#state.timers)
    end,
    {noreply, State#state{timers = Timers}};
handle_info(_Info, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%====================================================================
%% Hook callbacks
%%====================================================================
-spec iq_ping(iq()) -> iq().
iq_ping(#iq{type = get, sub_els = [#ping{}]} = IQ) ->
    xmpp:make_iq_result(IQ);
iq_ping(#iq{lang = Lang} = IQ) ->
    Txt = ?T("Ping query is incorrect"),
    xmpp:make_error(IQ, xmpp:err_bad_request(Txt, Lang)).

%% Activate pinging only to operators
user_online(SID, JID, _Info) ->	
    #jid{user = User, lserver = LServer} = JID,
    case mod_shared_roster:is_user_in_group({User, LServer}, <<"operators">>, LServer) of
        'false' -> ok;
        'true' ->
          ?INFO_MSG("user_online ~p SID: ~p start pinging",[JID#jid.user, SID]),
          start_ping(JID#jid.lserver, {SID, JID})
	end.

user_offline(SID, JID, _Info) ->
    ?INFO_MSG("user_offline ~p  SID: ~p stop pinging",[JID#jid.user, SID]),
    stop_ping(JID#jid.lserver, {SID, JID}).

%%====================================================================
%% Internal functions
%%====================================================================
init_state(Host, Opts) ->
    SendPings = mod_lhcping_opt:send_pings(Opts),
    PingInterval = mod_lhcping_opt:ping_interval(Opts),
    TimeoutAction = mod_lhcping_opt:timeout_action(Opts),
    PingAddress =  mod_lhcping_opt:ping_address(Opts),
    AHEnviroment =  mod_lhcping_opt:ahenviroment(Opts),
    BaseDomain =  mod_lhcping_opt:basedomain(Opts),
    AHProtocol =  mod_lhcping_opt:ahprotocol(Opts),
    #state{host = Host,
           send_pings = SendPings,
           ping_interval = PingInterval,
           timeout_action = TimeoutAction,
           ping_address = PingAddress,
           ahenviroment = AHEnviroment,
           basedomain = BaseDomain,
           ahprotocol = AHProtocol,
           timers = (?DICT):new()}.

register_hooks(Host) ->
    ejabberd_hooks:add(sm_register_connection_hook, Host,
                       ?MODULE, user_online, 100),
    ejabberd_hooks:add(sm_remove_connection_hook, Host,
                       ?MODULE, user_offline, 100).

unregister_hooks(Host) ->
    ejabberd_hooks:delete(sm_remove_connection_hook, Host,
                          ?MODULE, user_offline, 100),
    ejabberd_hooks:delete(sm_register_connection_hook, Host,
                          ?MODULE, user_online, 100).

register_iq_handlers(Host) ->
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_PING,
                                  ?MODULE, iq_ping),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_PING,
                                  ?MODULE, iq_ping).

unregister_iq_handlers(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_PING),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_PING).

add_timer({SID, JID} = ID, Interval, Timers) ->
    LJID = jid:tolower(JID),
    NewTimers = case (?DICT):find({SID, LJID}, Timers) of
		  {ok, OldTRef} ->
		      cancel_timer(OldTRef), (?DICT):erase({SID, LJID}, Timers);
		  _ -> Timers
		end,
    TRef = erlang:start_timer(Interval, self(),
			      {ping, ID}),
    (?DICT):store({SID, LJID}, TRef, NewTimers).

del_timer({SID, JID}, Timers) ->
    LJID = jid:tolower(JID),
    case (?DICT):find({SID, LJID}, Timers) of
      {ok, TRef} ->
	  cancel_timer(TRef),
          (?DICT):erase({SID, LJID}, Timers);
      _ -> Timers
    end.

cancel_timer(TRef) ->
    case erlang:cancel_timer(TRef) of
      false ->
	  receive {timeout, TRef, _} -> ok after 0 -> ok end;
      _ -> ok
    end.
    
mod_opt_type(ping_interval) ->
    econf:timeout(second);
mod_opt_type(send_pings) ->
    econf:bool();
mod_opt_type(timeout_action) ->
    econf:enum([none, kill]);
mod_opt_type(ping_address) ->
    econf:string();
mod_opt_type(ahenviroment) ->
    econf:bool();
mod_opt_type(basedomain) ->
    econf:string();
mod_opt_type(ahprotocol) ->
    econf:string().

mod_options(_Host) ->
    [{ping_interval, timer:minutes(1)},
     {send_pings, false},
     {timeout_action, none},
     {ping_address, <<"http://127.0.0.1/xmppservice/operatorstatus">>},
     {ahenviroment, 'false'},
     {basedomain, ""},
     {ahprotocol, "http://"}].

depends(_Host, _Opts) ->
    [].

mod_doc() ->
    #{desc =>
          ?T("This module implements support for "
             "Live Host Chat ping"),
      opts =>
          [{ping_interval,
            #{value => "timeout()",
              desc =>
                  ?T("How often to send pings to connected clients, "
                     "if option 'send_pings' is set to 'true'. If a client "
                     "connection does not send or receive any stanza "
                     "within this interval, a ping request is sent to "
                     "the client. The default value is '1' minute.")}},
           {send_pings,
            #{value => "true | false",
              desc =>
                  ?T("If this option is set to 'true', the server "
                     "sends pings to connected clients that are not "
                     "active in a given interval defined in 'ping_interval' "
                     "option. This is useful to keep client connections "
                     "alive or checking availability. "
                     "The default value is 'false'.")}},
           {timeout_action,
            #{value => "none | kill",
              desc =>
                  ?T("What to do when a client does not answer to a "
                     "server ping request in less than period defined "
                     "in 'ping_ack_timeout' option: "
                     "'kill' means destroying the underlying connection, "
                     "'none' means to do nothing. NOTE: when 'mod_stream_mgmt' "
                     "module is loaded and stream management is enabled by "
                     "a client, killing the client connection doesn't mean "
                     "killing the client session - the session will be kept "
                     "alive in order to give the client a chance to resume it. "
                     "The default value is 'none'.")}}],
      example =>
          ["modules:",
           "  ...",
           "  mod_lhcping:",
           "    send_pings: true",
           "    ping_interval: 4 min",
           "    timeout_action: kill",
           "  ..."]}.

