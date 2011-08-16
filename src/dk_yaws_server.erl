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

-export([start_link/0, run/0]).

%%%------------------------------------------------------------------------------
%% @doc
%%   Spawn a new process to start yaws via run/0. To properly configure
%%%------------------------------------------------------------------------------
start_link() ->
    {ok, proc_lib:spawn_link(?MODULE, run, [])}.

%%%------------------------------------------------------------------------------
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
%%%------------------------------------------------------------------------------
run() ->
    Id = "dk_yaws",
    Docroot = get_app_env(docroot, "/var/yaws/www"),
    GconfList = [{id, Id}],
    SconfList =
        [
         %% HTTP listener...
         {port, 8888},
         {listen, {0,0,0,0}},
         {docroot, Docroot}
        ],
    {ok, SCList, GC, ChildSpecs} =
        yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id),
    [supervisor:start_child(dk_yaws_sup, Ch) || Ch <- ChildSpecs],
    yaws_api:setconf(GC, SCList),
    {ok, self()}.


%%%------------------------------------------------------------------------------
%% @doc
%%   Get config parameter for the running application.
%%
%%   Check the current application context, then the init
%%   context, and finally return a default if neither has
%%   a value.
%% @end
%%%------------------------------------------------------------------------------
-spec get_app_env(atom(), any()) -> any().

get_app_env(Param, Default) ->
    case application:get_env(Param) of
        {ok, Val} -> Val;
        undefined ->
            case init:get_argument(Param) of
                {ok, [[FirstVal | _OtherVals], _MoreVals]} -> FirstVal;
                error -> Default
            end
    end.
