%%%------------------------------------------------------------------------------
%%% @copyright (c) 2011-2012, DuoMark International, Inc.  All rights reserved
%%% @author Jay Nelson <jay@duomark.com>
%%% @doc
%%%   The dk_yaws_server configures embedded yaws so that an including
%%%   application can set the configuration parameters in its own
%%%   application configuration file.
%%% @since v0.0.1
%%% @end
%%%------------------------------------------------------------------------------
-module(dk_yaws_server).
-copyright("(c) 2011-2012, DuoMark International, Inc.  All rights reserved").
-author(jayn).

%% External API
-export([start_link/0, run/0]).

%% Testing API
-export([get_params/1]).

-include("dk_yaws_params.hrl").


%%%==============================================================================
%%% External API
%%%==============================================================================

-spec start_link() -> {ok, pid()}.
-spec run() -> {ok, pid()}.


%% @doc Spawn a new process to start yaws with a proper configuration via run/0.
start_link() ->
    {ok, proc_lib:spawn_link(?MODULE, run, [])}.


%%-------------------------------------------------------------------------------
%% @doc
%%   Use application environment parameters to determine the port for yaws
%%   to listen on and the root of the document hierarchy on disk from which
%%   yaws will serve data files. If no values are accessible, the code is
%%   hardwired to use port 8888 and a docroot of '/var/yaws/www'.
%%
%%   A call to yaws_api:embedded_start_conf/4 constructs the child specs
%%   needed to allow dk_yaws_sup to start_child processes for a functioning
%%   embedded yaws installation.
%%
%%   The run/0 function ends after successfully launching new yaws child
%%   processes, relying on dk_yaws_sup to keep them running.
%% @end
%%-------------------------------------------------------------------------------
run() ->
    GconfList = [{id, ?APP_ID}],
    SconfList = get_params(?PARAM_LIST),
    Docroot = proplists:get_value(?APP_PARAM_DOCROOT, SconfList),
    {ok, SCList, GC, ChildSpecs} =
        yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, ?APP_ID),
    [supervisor:start_child(dk_yaws_sup, Ch) || Ch <- ChildSpecs],
    yaws_api:setconf(GC, SCList),
    {ok, self()}.


%%%==============================================================================
%%% Internal functions
%%%==============================================================================

-type default_prop() :: {atom(), any()}.
-spec get_params(list(default_prop())) -> list(proplists:property()).

%% @private
%% @doc Retrieve the application environment parameters or supply defaults.
get_params(Params) ->
    Proplist = [{Param, dk_utils:get_app_env(Param, Default)}
                || {Param, Default} <- Params],
    normalize_ip_and_port(Proplist).

%% @private
%% @doc Ensure IP is a tuple and port is an integer
normalize_ip_and_port(Proplist) ->    
    IpTuple = normalize_ip(Proplist),
    normalize_port(IpTuple).

normalize_ip(Proplist) ->
    {IpPair, Rest} = proplists:split(Proplist, [?APP_PARAM_IP]),
    case IpPair of
        [[{?APP_PARAM_IP, Ip}]] when is_tuple(Ip) -> Proplist;
        [[{?APP_PARAM_IP, Ip}]] ->
            [{?APP_PARAM_IP, convert_ip(Ip)} | Rest]
    end.
    
convert_ip(Ip) when is_list(Ip) ->            
    IpParts = string:tokens(Ip, "."),
    case length(IpParts) of
        4 -> list_to_tuple([list_to_integer(N) || N <- IpParts]);
        _Improper -> ?DEFAULT_IP_TUPLE
    end.

normalize_port(Proplist) ->
    {PortPair, Rest} = proplists:split(Proplist, [?APP_PARAM_PORT]),
    case PortPair of
        [[{?APP_PARAM_PORT, Port}]] when is_integer(Port) -> Proplist;
        [[{?APP_PARAM_PORT, Port}]] ->
            [{?APP_PARAM_PORT, convert_port(Port)} | Rest]
    end.

convert_port(Port) when is_list(Port) ->
    list_to_integer(Port).
