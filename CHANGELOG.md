# Summoner

1.0.0
=====

* __Important:__ Rename `hs-init` to `summoner`. Transform the script into the package.
* [#54](https://github.com/kowainik/summoner/issues/54):
   Rename `on` and `off` commands to `with` and `without`.
* [#48](https://github.com/kowainik/summoner/issues/48):
  - Add ability to write configurations file. Remove `Targets` data type.
    Use `Config` instead for default, file and CLI configurations.
* [#60](https://github.com/kowainik/summoner/issues/60):
  Use custom prelude `universum`.
* [#39](https://github.com/kowainik/summoner/issues/39):
  Add option to use custom prelude in CLI and in `.toml` configuration.
* [#38](https://github.com/kowainik/summoner/issues/38):
  Add option to add default extensions to `.toml` config.

# hs-init

0.0.4
=====
* Now works on windows systems as well
* Add powershell install script for Windows


0.0.3
=====

* [#45](https://github.com/vrom911/hs-init/issues/45):
  Support AppVeyor CI for created projects.

0.0.2
=====

* Use metavar for on-off commands.
* Upgrade `lts` to `10.3`, use `ghc 8.2.2` as default value.

0.0.1
=====

* [#36](https://github.com/vrom911/hs-init/issues/36):
  Add option for creating private repositories — `-p | --private`.
