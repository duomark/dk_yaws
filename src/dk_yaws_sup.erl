%%%------------------------------------------------------------------------------
%%% @copyright (c) 2011, DuoMark International, Inc.  All rights reserved
%%% @author Jay Nelson <jay@duomark.com>
%%% @doc
%%%   The dk_yaws_sup supervisor serves as the root of the yaws hierarchy,
%%%   defining a single supervisor to use as an included application.
%%% @since v0.0.1
%%% @end
%%%------------------------------------------------------------------------------
-module(dk_yaws_sup).
-copyright("(c) 2011, DuoMark International, Inc.  All rights reserved").
-author(jayn).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


%%===============================================================================
%% API functions
%%===============================================================================

-spec start_link() -> {ok, pid()}.

start_link() ->
    ssl:start(),
    inets:start(),
    supervisor:start_link({local, ?SERVER}, ?MODULE, {}).


%%===============================================================================
%% Supervisor callbacks
%%===============================================================================

-spec init(Args::{}) -> {ok, any()}.

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Args), {I, {I, start_link, Args}, transient, 5000, worker, [I]}).

init({}) ->
    YawsServer = ?CHILD(dk_yaws_server, []),
    {ok, { {one_for_all, 5, 10}, [YawsServer]} }.
