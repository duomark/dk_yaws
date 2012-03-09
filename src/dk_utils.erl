%%%------------------------------------------------------------------------------
%%% @copyright (c) 2011, DuoMark International, Inc.  All rights reserved
%%% @author Jay Nelson <jay@duomark.com>
%%% @doc
%%%   Useful utility functions for all DuoMark (dk_*) products.
%%% @since v0.1.1
%%% @end
%%%------------------------------------------------------------------------------
-module(dk_utils).
-copyright("(c) 2011, DuoMark International, Inc.  All rights reserved").
-author(jayn).

%% External API
-export([get_app_env/2]).


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
                {ok, [[FirstVal | _OtherVals] | _MoreVals]} -> FirstVal;
                error -> Default
            end
    end.
