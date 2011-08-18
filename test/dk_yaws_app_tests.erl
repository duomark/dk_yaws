%%%------------------------------------------------------------------------------
%%% @copyright (c) 2011, DuoMark International, Inc.  All rights reserved
%%% @author Jay Nelson <jay@duomark.com>
%%% @doc
%%%   Eunit tests for cosb_svcnode_app.
%%% @since v0.0.3
%%% @end
%%%------------------------------------------------------------------------------
-module(dk_yaws_app_tests).
-copyright("(c) 2011, DuoMark International, Inc.  All rights reserved").
-author(jayn).

-include_lib("eunit/include/eunit.hrl").

-define(TM, dk_yaws_app).
-define(ROOT_SUPER, dk_yaws_sup).

start_app() ->
    fun() ->
            ok = meck:new(?ROOT_SUPER),
            register(sup_count, proc_lib:spawn_link(fun() -> calls(0) end)),
            meck:expect(?ROOT_SUPER, start_link,
                        fun() ->
                                sup_count ! {inc, self()},
                                {ok, proc_lib:spawn_link(fun() -> loop() end)}
                        end),
            ?TM:start(takeover, none)
    end.

stop_app() ->
    fun({ok, P1}) ->
            Pid = whereis(sup_count), unregister(sup_count),
            process_flag(trap_exit, true),
            unlink(Pid), exit(Pid, kill),
            meck:unload(?ROOT_SUPER),
            unlink(P1),
            case exit(P1, shutdown) of
                true -> ok
            end,
            ok
    end.

loop() ->
    receive Any -> Any
    after 5000 -> ok
    end.

calls(CallCount) ->
    receive
        {inc, _From}  -> calls(CallCount+1);
        {count, From} -> From ! {count, CallCount}, calls(CallCount)
    after 5000 -> ok
    end.


%% @doc Start the server and verify the children.
start_test_() ->
    {setup, start_app(), stop_app(),
     {with, [fun check_start/1, fun check_stop/1]}
    }.

check_start({ok, P1}) when is_pid(P1) ->
    ?assert(is_process_alive(P1)),
    sup_count ! {count, self()},
    receive {count, C} -> ?assertMatch(1, C)
    after 1000 -> throw(bad_sup_launch_count)
    end,
    ok.
        
check_stop({ok, P1}) when is_pid(P1) ->
    ?assert(is_process_alive(P1)),
    ?assertMatch(ok, ?TM:stop([])).
