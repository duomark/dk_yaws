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

-export([
         check_start/1
        ]).

-include_lib("eunit/include/eunit.hrl").

-define(TM, dk_yaws_server).

start() -> fun() -> ?TM:start_link() end.
stop() ->  fun({ok, _P1}) -> ok end.


%% @doc Start the server and verify the children.
start_test_() ->
    {setup, start(), stop(),
     {with, [fun check_start/1]}
    }.

check_start({ok, P1}) when is_pid(P1) ->
    ?assert(is_process_alive(P1)).
