-module(mod_lhc).

-behavior(gen_mod).
-include("ejabberd.hrl").
-include("logger.hrl").
-include("ejabberd.hrl").
-include("jlib.hrl").

-export([start/2, stop/1, on_set/4, on_unset/4]).

start(Host, _Opts) ->
   ejabberd_hooks:add(set_presence_hook, Host, ?MODULE, on_set, 50),
   ejabberd_hooks:add(unset_presence_hook, Host, ?MODULE, on_unset, 50),
   ok.

stop(Host) ->
   ejabberd_hooks:delete(set_presence_hook, Host, ?MODULE, on_set, 50),
   ejabberd_hooks:delete(unset_presence_hook, Host, ?MODULE, on_unset, 50),
   ok.

on_set(User, Server, Resource, Packet) ->
   LUser = jlib:nodeprep(User),
   LServer = jlib:nodeprep(Server),
   SID = ejabberd_sm:get_session_pid(LUser, LServer, Resource),  
   ?INFO_MSG("Presence set %s %s", [SID,LUser]).

on_unset(User, Server, Resource, Packet) ->
   LUser = jlib:nodeprep(User),
   LServer = jlib:nodeprep(Server),
   ?INFO_MSG("Presence un-set %s %s", [LUser]). 	
