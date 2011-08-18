dk_yaws (DuoMark Yaws Application)
==================================

Yaws is an erlang application that implements a sophisticated web server. It can be used standalone or as a server embedded in another application. dk_yaws provides a simple application that manages yaws, and it can serve as the basis for embedding yaws into another application.

This project uses rebar. It demonstrates how to build your own application and use it as an included application in another project using rebar.

Travis CI
=========

[![Build Status](http://travis-ci.org/duomark/dk_yaws.png)](http://travis-ci.org/duomark/dk_yaws])

This project is set up to use [Travis-CI](http://about.travis-ci.org/) for Continuous Integration. After a check in which modifies the code, Travis will build and run the unit tests to verify that the code still works. The status of the current build is shown in the above image badge.

Integration with Travis is provided by the [.travis.yml file](https://raw.github.com/duomark/dk_yaws/master/.travis.yml). Note there is a special 'before script' installation of PAM required by Yaws or it cannot build on their worker nodes. The automated build is run on R14B01, R14B02 and R14B03.

Included software
=================

This project is built with dependencies on other open source software projects.

The following software is included in the deployed application bundle:

  * yaws-1.90 (git://github.com/klacke/yaws.git)

The following software is used to build, test or validate the application during development:

  * meck (git://github.com/eproxus/meck.git)

Running dk_yaws standalone
==========================

Download and install the source code, then perform the following at the command line:

  1. make realclean all rel
  1. rel/dk_yaws/bin/dk_yaws console

You will now have a webserver running and listening on port 8000 as specified in the [app.config file](https://raw.github.com/duomark/dk_yaws/master/rel/files/app.config) and [vars.config file](https://raw.github.com/duomark/dk_yaws/master/rel/files/vars.config) with dk_yaws as the primary application.

Embedding dk_yaws
=================

There are just a few steps to embed dk_yaws in another application. To understand them, you need a little background on OTP applications, included applications and supervisors. The following description is a summary of the information in the standard OTP documentation at http://www.erlang.org/doc/design_principles/included_applications.html#id73606 as it relates to an embedded erlang node (one in which all code is loaded on startup, guaranteeing the availability of all the functions or refusing to start up).

There are two kinds of OTP applications: the primary application and included applications. There can only be one primary application. It has an application callback module (traditionally xxx_app.erl), that is invoked by the application controller when the node is booted. It should do no special customizations, and should invoke a single root supervisor to start all subsidiary applications and components.

An included application is one which can exist as a primary application, but is instead made a subordinate component of the primary application. The primary application invokes the root supervisor of an included application within any of its own supervisors (but only once!) so that all dependencies from the root supervisor down become a subcomponent of the primary application. The application callback module is unused for an included application.

Running dk_yaws as an included application is defined by:

  1. Adding {included_applications, [dk_yaws]} in the primary application's .app.src
  1. Calling dk_yaws_sup:start_link() from exactly one of the primary application's supervisors

If you look at the [dk_yaws.app.src file](https://raw.github.com/duomark/dk_yaws/master/src/dk_yaws.app.src) you can see that yaws is an included_application when dk_yaws is the primary application.

