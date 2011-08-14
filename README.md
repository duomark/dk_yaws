dk_yaws (DuoMark Yaws Application)
==================================

Yaws is an erlang application that implements a sophisticated web server. It can be used standalone or as a server embedded in another application. dk_yaws provides a simple application that manages yaws, and it can serve as the basis for embedding yaws into another application.

This project uses rebar. It demonstrates how to build your own application and use it as an included application in another project using rebar.

Running dk_yaws standalone
==========================

Download and install the source code, then perform the following at the command line:

  # make realclean all rel
  # rel/dk_yaws/bin/dk_yaws console

You will now have a webserver running and listening on the default port 8888.

Embedding dk_yaws
=================

