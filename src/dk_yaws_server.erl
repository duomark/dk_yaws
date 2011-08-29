%%%------------------------------------------------------------------------------
%%% @copyright (c) 2011, DuoMark International, Inc.  All rights reserved
%%% @author Jay Nelson <jay@duomark.com>
%%% @doc
%%%   The dk_yaws_server configures embedded yaws so that an including
%%%   application can set the configuration parameters in its own
%%%   application configuration file.
%%% @since v0.0.1
%%% @end
%%%------------------------------------------------------------------------------
-module(dk_yaws_server).
-copyright("(c) 2011, DuoMark International, Inc.  All rights reserved").
-author(jayn).

%% External API
-export([start_link/0, run/0]).

-include("dk_yaws_params.hrl").


%%%==============================================================================
%%% External API
%%%==============================================================================

-spec start_link() -> {ok, pid()}.
-spec run() -> {ok, pid()}.


%% @doc Spawn a new process to start yaws via run/0. To properly configure
start_link() ->
    {ok, proc_lib:spawn_link(?MODULE, run, [])}.

%%-------------------------------------------------------------------------------
%% @doc
%%   Use application environment parameters to determine the port for yaws
%%   to listen on and the root of the document hierarchy on disk from which
%%   yaws will serve data files. If no values are accessible, the code is
%%   hardwired to use port 8888 and a docroot of '/var/yaws/www'.
%%
%%   A call to yaws_api:embedded_start_conf/4 is used to construct the child
%%   specs needed to allow dk_yaws_sup to start_child processes for a
%%   functioning embedded yaws installation.
%%
%%   The run/0 function ends after successfully launching new yaws child
%%   processes, relying on dk_yaws_sup to keep them running.
%% @end
%%-------------------------------------------------------------------------------
run() ->
    Docroot = dk_utils:get_app_env(?APP_PARAM_DOCROOT, ?DEFAULT_DOCROOT),
    GconfList = [{id, ?APP_ID}],
    SconfList = get_ip_and_port() ++ [{docroot, Docroot}],
    {ok, SCList, GC, ChildSpecs} =
        yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, ?APP_ID),
    [supervisor:start_child(dk_yaws_sup, Ch) || Ch <- ChildSpecs],
    yaws_api:setconf(GC, SCList),
    {ok, self()}.


%%%==============================================================================
%%% Internal functions
%%%==============================================================================

%% @private
%% @doc Get the IP and Port from this application's configuration parameters.
get_ip_and_port() ->
    Ip = dk_utils:get_app_env(?APP_PARAM_IP, ?DEFAULT_IP),
    IpParts = string:tokens(Ip, "."),
    IpTuple = case length(IpParts) of
                  4 -> list_to_tuple([list_to_integer(N) || N <- IpParts]);
                  _Improper -> ?DEFAULT_IP_TUPLE
              end,
    Port = dk_utils:get_app_env(?APP_PARAM_PORT, ?DEFAULT_PORT),
    [{listen, IpTuple}, {port, Port}].
