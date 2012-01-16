%%%------------------------------------------------------------------------------
%%% @copyright (c) 2011-2012, DuoMark International, Inc.  All rights reserved
%%% @author Jay Nelson <jay@duomark.com>
%%% @doc
%%%   Verify that dk_yaws_server instantiates and operates properly.
%%% @since v0.1.1
%%% @end
%%%------------------------------------------------------------------------------
-module(dk_yaws_server_tests).
-copyright("(c) 2011-2012, DuoMark International, Inc.  All rights reserved").
-author(jayn).

-include_lib("eunit/include/eunit.hrl").
-include("dk_yaws_params.hrl").

%% Declare the test module and application parameters.
-define(TM, dk_yaws_server).


%% @doc Verify application parameter handling.
params_test_() ->
    [fun check_params/0].

%% @doc Verify IP and port are properly formatted.
check_params() ->
    meck_default_params(),
    Params1 = ?TM:get_params(?PARAM_LIST),
    ?assertMatch({0,0,0,0}, proplists:get_value(?YAWS_PARAM_IP, Params1)),
    ?assertMatch(8888, proplists:get_value(?YAWS_PARAM_PORT, Params1)),
    unmeck_params(),
    
    meck_string_params(),
    Params2 = ?TM:get_params(?PARAM_LIST),
    ?assertMatch({127,0,0,1}, proplists:get_value(?YAWS_PARAM_IP, Params2)),
    ?assertMatch(7777, proplists:get_value(?YAWS_PARAM_PORT, Params2)),
    unmeck_params().
    

%% @doc Test that calling run/0 starts a yaws server.
start_test_() ->
    [fun check_run/0].

%% @doc Verify that run/0 sets all parameters, then sets up a yaws server properly.
check_run() ->
    meck_run_apis(),
    clear_param_counts(),
    ?TM:run(),
    check_params_set_just_once(),
    unmeck_run_apis().


%%%==============================================================================
%%% Support functions
%%%==============================================================================

unmeck_params() -> meck:unload(dk_utils).
meck_default_params() ->
    meck:new(dk_utils),
    ok = meck:expect(dk_utils, get_app_env, fun default_params/2).
meck_string_params() ->
    meck:new(dk_utils),
    ok = meck:expect(dk_utils, get_app_env, fun string_params/2).

string_params(?APP_PARAM_IP, _Any) ->   "127.0.0.1";
string_params(?APP_PARAM_PORT, _Any) -> "7777";
string_params(Name, Value) ->
    default_params(Name, Value).

default_params(_Name, Value) -> Value.


meck_run_apis() ->
    meck:new(dk_utils),
    ok = meck:expect(dk_utils, get_app_env, fun ret_once/2),
    meck:new(yaws_api),
    ok = meck:expect(yaws_api, embedded_start_conf, fun check_yaws_args/4),
    ok = meck:expect(yaws_api, setconf, fun(_GC, _SCList) -> ok end),
    code:unstick_dir(code:lib_dir(stdlib) ++ "/ebin/"),
    meck:new(supervisor),
    ok = meck:expect(supervisor, start_child, fun check_sup_args/2).

unmeck_run_apis() ->
    [meck:unload(M) || M <- [supervisor, yaws_api, dk_utils]],
    code:stick_dir(code:lib_dir(stdlib) ++ "/ebin/").
    
%% Count the number of times parameters are set with a default value.
ret_once(?APP_PARAM_DOCROOT = K, ?DEFAULT_DOCROOT = V) -> put(K, get(K)+1), V;
ret_once(?APP_PARAM_PORT    = K, ?DEFAULT_PORT    = V) -> put(K, get(K)+1), V;
ret_once(?APP_PARAM_IP      = K, ?DEFAULT_IP      = V) -> put(K, get(K)+1), V.

%% Verify that yaws instantiates correctly.
check_yaws_args(?DEFAULT_DOCROOT, _Sconf, [{id, ?APP_ID}], ?APP_ID) -> {ok, [], [], []}.
check_sup_args(dk_yaws_sup, _ChildSpec) -> {ok, self()}.
    
%% Track parameter accesses.
all_params() -> [?APP_PARAM_DOCROOT, ?APP_PARAM_PORT, ?APP_PARAM_IP].
clear_param_counts() -> [put(K, 0) || K <- all_params()].
check_params_set_just_once() ->
    [begin
         ?assertMatch({K, 1}, {K, get(K)}),
         put(K, undefined)
     end || K <- all_params()].
    
