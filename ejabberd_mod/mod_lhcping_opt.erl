%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_lhcping_opt).

-export([ahenviroment/1]).
-export([ahprotocol/1]).
-export([basedomain/1]).
-export([ping_address/1]).
-export([ping_interval/1]).
-export([send_pings/1]).
-export([timeout_action/1]).

-spec ahenviroment(gen_mod:opts() | global | binary()) -> boolean().
ahenviroment(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ahenviroment, Opts);
ahenviroment(Host) ->
    gen_mod:get_module_opt(Host, mod_lhcping, ahenviroment).

-spec ahprotocol(gen_mod:opts() | global | binary()) -> string().
ahprotocol(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ahprotocol, Opts);
ahprotocol(Host) ->
    gen_mod:get_module_opt(Host, mod_lhcping, ahprotocol).

-spec basedomain(gen_mod:opts() | global | binary()) -> string().
basedomain(Opts) when is_map(Opts) ->
    gen_mod:get_opt(basedomain, Opts);
basedomain(Host) ->
    gen_mod:get_module_opt(Host, mod_lhcping, basedomain).

-spec ping_address(gen_mod:opts() | global | binary()) -> <<_:344>> | string().
ping_address(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ping_address, Opts);
ping_address(Host) ->
    gen_mod:get_module_opt(Host, mod_lhcping, ping_address).

-spec ping_interval(gen_mod:opts() | global | binary()) -> pos_integer().
ping_interval(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ping_interval, Opts);
ping_interval(Host) ->
    gen_mod:get_module_opt(Host, mod_lhcping, ping_interval).

-spec send_pings(gen_mod:opts() | global | binary()) -> boolean().
send_pings(Opts) when is_map(Opts) ->
    gen_mod:get_opt(send_pings, Opts);
send_pings(Host) ->
    gen_mod:get_module_opt(Host, mod_lhcping, send_pings).

-spec timeout_action(gen_mod:opts() | global | binary()) -> 'kill' | 'none'.
timeout_action(Opts) when is_map(Opts) ->
    gen_mod:get_opt(timeout_action, Opts);
timeout_action(Host) ->
    gen_mod:get_module_opt(Host, mod_lhcping, timeout_action).

