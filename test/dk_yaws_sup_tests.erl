%%%------------------------------------------------------------------------------
%%% @copyright (c) 2011, DuoMark International, Inc.  All rights reserved
%%% @author Jay Nelson <jay@duomark.com>
%%% @doc
%%%   Verify that dk_yaws_sup instantiates and operates properly.
%%% @since v0.0.3
%%% @end
%%%------------------------------------------------------------------------------
-module(dk_yaws_sup_tests).
-copyright("(c) 2011, DuoMark International, Inc.  All rights reserved").
-author(jayn).

-export([
         start_supervisor/0,
         check_children/1
        ]).

-include_lib("eunit/include/eunit.hrl").

-define(TM, dk_yaws_sup).

start_supervisor() ->
    fun() -> ?TM:start_link() end.
stop_supervisor() -> 
    fun({ok, P1}) ->
            unlink(P1), unregister(?TM),
            process_flag(trap_exit, true),
            case exit(P1, kill) of true -> ok end
    end.


%% @doc Start the server and verify the children.
start_test_() ->
    {setup, start_supervisor(), stop_supervisor(),
     {with, [fun check_children/1]}
    }.

check_children({ok, P1}) when is_pid(P1) ->
    ?assert(is_process_alive(P1)),

    ChildCounts = supervisor:count_children(P1),
    ?assertEqual(1, proplists:get_value(specs, ChildCounts)),
    ?assertEqual(1, proplists:get_value(active, ChildCounts)),
    ?assertEqual(0, proplists:get_value(supervisors, ChildCounts)),
    ?assertEqual(1, proplists:get_value(workers, ChildCounts)),

    ExpectedChildren = [],
    GoodKids = [Id || {Id, _, _, [Id]} <- supervisor:which_children(P1),
                      lists:member(Id, ExpectedChildren)],

    ?assertEqual(length(ExpectedChildren), length(GoodKids)).
