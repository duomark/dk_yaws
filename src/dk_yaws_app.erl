%%%------------------------------------------------------------------------------
%%% @copyright (c) 2011, DuoMark International, Inc.  All rights reserved
%%% @author Jay Nelson <jay@duomark.com>
%%% @doc
%%%   The dk_yaws_app is used for standalone launch of yaws. To make it
%%%   embedded, invoke dk_yaws_sup:start_link() from the including
%%%   application's supervisor and list dk_yaws as an included application
%%%   in the .app.src file.
%%% @since v0.0.1
%%% @end
%%%------------------------------------------------------------------------------
-module(dk_yaws_app).
-copyright("(c) 2011, DuoMark International, Inc.  All rights reserved").
-author(jayn).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).


%%===============================================================================
%% Application callbacks
%%===============================================================================

-spec start() -> {ok, pid()}.
-spec start(any(), any()) -> {ok, pid()}.
-spec stop([]) -> ok.

%% @doc Start the application's root supervisor in erl listener.
start() ->
    dk_yaws_sup:start_link().

%% @doc Start the application's root supervisor from boot.
start(_StartType, _StartArgs) ->
    dk_yaws_sup:start_link().

%% @doc Stop the application.
stop(_State) -> ok.
