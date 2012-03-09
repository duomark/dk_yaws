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

%% Hack because inets doesn't have a root supervisor start_link/0 call.
%% Planning a patch to OTP to address this.
-export([start_link/1]).

-define(SERVER, ?MODULE).


%%===============================================================================
%% API functions
%%===============================================================================

-spec start_link() -> {ok, pid()}.
-spec start_link(inets) -> {ok, pid()}.

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {}).

start_link(inets) ->
    supervisor:start_link({local, inets_sup}, inets_sup, []).


%%===============================================================================
%% Supervisor callbacks
%%===============================================================================

-spec init(Args::{}) -> {ok, {tuple(), list()}}.


%% Helper macro for declaring children of supervisor
-define(SUPER(__Mod, __Args), {__Mod, {__Mod, start_link, __Args}, permanent, infinity, supervisor, [__Mod]}).
-define(CHILD(__Mod, __Args), {__Mod, {__Mod, start_link, __Args}, transient, 5000, worker, [__Mod]}).

%% Hack required by lack of inets_sup:start_link/0
-define(SUPER(__Name, __Mod, __Args), {__Name, {__Mod, start_link, __Args}, permanent, infinity, supervisor, [__Name]}).

init({}) ->
    CryptoSup = ?SUPER(crypto_sup, []),
    SslSup = ?SUPER(ssl_sup, []),
    InetsSup = ?SUPER(inets_sup, dk_yaws_sup, [inets]),
    YawsServer = ?CHILD(dk_yaws_server, []),
    {ok, { {one_for_one, 5, 10}, [
                                  CryptoSup,
                                  SslSup,
                                  InetsSup,
                                  YawsServer]} }.
