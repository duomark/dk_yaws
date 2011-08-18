0.1.0
=====
Release pending

  * Turned on Continuous Integration using http://travis-ci.org/
  * Added software dependencies to README.md
  * Added new BSD (simplified 2-clause) LICENSE
  * Added release history (RELEASES.md) and acknowledgements (CONTRIBUTORS.md)
  * Added rebar executable solely for Travis CI to work properly

0.0.3
=====
Released 2011-08-18

  * Added meck 0.7.1 as a dependency in rebar.config
  * Added initial unit tests for dk_yaws_app and dk_yaws_sup
  * Establish rel/vars.config as the template value supplier for rel/app.config and rel/vm.args

0.0.2
=====
Released 2011-08-16

  * More descriptive documentation on embedded yaws versus standalone yaws
  * Changed app.config parameters to use 'dk_yaws_' as a prefix

0.0.1
=====
Released 2011-08-15

  * Initial ability to launch yaws standalone under the dk_yaws_sup supervisor
  * Creation of the rel/dk_yaws/bin/dk_yaws boot script for command line operation
