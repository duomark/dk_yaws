%%%------------------------------------------------------------------------------
%%% @copyright (c) 2011, DuoMark International, Inc.  All rights reserved
%%% @author Jay Nelson <jay@duomark.com>
%%% @doc
%%%   Verify that dk_yaws_server instantiates and operates properly.
%%% @since v0.1.1
%%% @end
%%%------------------------------------------------------------------------------
-module(dk_yaws_server_tests).
-copyright("(c) 2011, DuoMark International, Inc.  All rights reserved").
-author(jayn).

%% -export([ %% check_sup_args/2,
%%          check_params/0]).

-include_lib("eunit/include/eunit.hrl").

%% Declare the test module and application parameters.
-define(TM, dk_yaws_server).
-include("dk_yaws_params.hrl").

%% @doc Test that calling run/0 starts a yaws server.
run_test_() -> [fun check_run/0].

check_run() ->
    meck:new(dk_utils, [passthrough]),
    ok = meck:expect(dk_utils, get_app_env, fun ret_once/2),
    meck:new(yaws_api),
    ok = meck:expect(yaws_api, embedded_start_conf, fun check_yaws_args/4),
    ok = meck:expect(yaws_api, setconf, fun(_GC, _SCList) -> ok end),
    code:unstick_dir(code:lib_dir(stdlib) ++ "/ebin/"),
    meck:new(supervisor),
    ok = meck:expect(supervisor, start_child, fun check_sup_args/2),

    %% Verify that run/0 sets all parameters, then sets up a yaws server properly.
    set_once(),
    ?TM:run(),
    check_params(),

    meck:unload(supervisor),
    code:stick_dir(code:lib_dir(stdlib) ++ "/ebin/"),
    meck:unload(yaws_api),
    meck:unload(dk_utils).


%% Init parameter accesses to 0.
all_params() -> [?APP_PARAM_DOCROOT, ?APP_PARAM_PORT, ?APP_PARAM_IP].
set_once() -> [put(K, 0) || K <- all_params()].

%% Count the number of times parameters are set.
ret_once(?APP_PARAM_DOCROOT = K, ?DEFAULT_DOCROOT = V) -> put(K, get(K)+1), V;
ret_once(?APP_PARAM_PORT    = K, ?DEFAULT_PORT    = V) -> put(K, get(K)+1), V;
ret_once(?APP_PARAM_IP      = K, ?DEFAULT_IP      = V) -> put(K, get(K)+1), V.

%% Ensure all parameters are set just once.
check_params() -> [begin ?assertMatch({K, 1}, {K, get(K)}), put(K, undefined) end || K <- all_params()].

%% Verify that yaws instantiates correctly.
check_yaws_args(?DEFAULT_DOCROOT, _Sconf, _Gconf, ?APP_ID) -> {ok, [], [], []}.
check_sup_args(dk_yaws_sup, _ChildSpec) -> {ok, self()}.
    

%% @doc Start the server and verify the children.
-define(RUN_RETURN, run_ran_last).
start_test_() -> [fun check_start/0].

check_start() ->
    meck:new(?TM, [passthrough]),
    ok = meck:expect(dk_yaws_server, run, fun() -> ?RUN_RETURN end),
    ?assertMatch({ok, P1} when is_pid(P1), ?TM:start_link()),
    meck:unload(?TM).
