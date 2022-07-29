%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_lhc_opt).

-export([ahenviroment/1]).
-export([ahprotocol/1]).
-export([basedomain/1]).
-export([login_address/1]).
-export([logout_address/1]).
-export([message_address/1]).

-spec ahenviroment(gen_mod:opts() | global | binary()) -> boolean().
ahenviroment(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ahenviroment, Opts);
ahenviroment(Host) ->
    gen_mod:get_module_opt(Host, mod_lhc, ahenviroment).

-spec ahprotocol(gen_mod:opts() | global | binary()) -> string().
ahprotocol(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ahprotocol, Opts);
ahprotocol(Host) ->
    gen_mod:get_module_opt(Host, mod_lhc, ahprotocol).

-spec basedomain(gen_mod:opts() | global | binary()) -> string().
basedomain(Opts) when is_map(Opts) ->
    gen_mod:get_opt(basedomain, Opts);
basedomain(Host) ->
    gen_mod:get_module_opt(Host, mod_lhc, basedomain).

-spec login_address(gen_mod:opts() | global | binary()) -> <<_:344>> | string().
login_address(Opts) when is_map(Opts) ->
    gen_mod:get_opt(login_address, Opts);
login_address(Host) ->
    gen_mod:get_module_opt(Host, mod_lhc, login_address).

-spec logout_address(gen_mod:opts() | global | binary()) -> <<_:336>> | string().
logout_address(Opts) when is_map(Opts) ->
    gen_mod:get_opt(logout_address, Opts);
logout_address(Host) ->
    gen_mod:get_module_opt(Host, mod_lhc, logout_address).

-spec message_address(gen_mod:opts() | global | binary()) -> <<_:344>> | string().
message_address(Opts) when is_map(Opts) ->
    gen_mod:get_opt(message_address, Opts);
message_address(Host) ->
    gen_mod:get_module_opt(Host, mod_lhc, message_address).

