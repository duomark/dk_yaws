
-define(APP_ID,            "dk_yaws").

-define(APP_PARAM_IP,      dk_yaws_ip).
-define(YAWS_PARAM_IP,     listen).
-define(DEFAULT_IP,        "0.0.0.0").
-define(DEFAULT_IP_TUPLE,  {0,0,0,0}).

-define(APP_PARAM_PORT,    dk_yaws_port).
-define(YAWS_PARAM_PORT,   port).
-define(DEFAULT_PORT,      8888).

-define(APP_PARAM_DOCROOT, dk_yaws_docroot).
-define(DEFAULT_DOCROOT,   "/var/yaws/www").

-define(PARAM_LIST, [{?APP_PARAM_IP,      ?DEFAULT_IP},
                     {?APP_PARAM_PORT,    ?DEFAULT_PORT},
                     {?APP_PARAM_DOCROOT, ?DEFAULT_DOCROOT}]).
