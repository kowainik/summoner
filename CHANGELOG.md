# Summoner

1.2.0
=======

* [#154](https://github.com/kowainik/summoner/issues/154):
* Add `Link` constructor to `Source` data type.

1.1.0.1
=======

* Bump up `relude` version to `0.3.0`.
* [#146](https://github.com/kowainik/summoner/issues/146):
  Migrate to `base-noprelude`.

1.1.0
=====

* [#128](https://github.com/kowainik/summoner/issues/128):
  __Important:__ Replace `summon` with `summon new` command.
  To create a project now the following command should be used:
  ```
  summon new my-project
  ```
* [#82](https://github.com/kowainik/summoner/issues/82):
  Add stackage badges.
* [#109](https://github.com/kowainik/summoner/issues/109):
  Fix travis cache directories.
* [#117](https://github.com/kowainik/summoner/issues/117):
  Step into created project properly
* [#110](https://github.com/kowainik/summoner/issues/110):
  Print executing command.
* [#91](https://github.com/kowainik/summoner/issues/91):
  Add warning fields for ghc.
* [#90](https://github.com/kowainik/summoner/issues/90):
  Add TOML test.
* [#120](https://github.com/kowainik/summoner/issues/120):
  Bump up dependencies.
* [#58](https://github.com/kowainik/summoner/issues/58):
  Make `Licence` type safer.
* [#124](https://github.com/kowainik/summoner/issues/124):
  Print directories bold.
* [#130](https://github.com/kowainik/summoner/issues/130):
  Show git revision version under `--version` command.
* [#63](https://github.com/kowainik/summoner/issues/63):
  Add `show ghc`, `show license` and `show license <LICENSE_NAME>` commands.
* [#122](https://github.com/kowainik/summoner/issues/122):
  Remove github link from CHANGELOG when github is not chosen.
* [#35](https://github.com/kowainik/summoner/issues/35):
  Add config option to add `.stylish-haskell.yaml` to your project
  (via URL or file path).
* Bump up `tomland` version to `0.4.0`.
* [#138](https://github.com/kowainik/summoner/issues/138):
  Remove traces of GitHub from `.cabal` file when GitHub integration is disabled.
* [#31](https://github.com/kowainik/summoner/issues/31):
  Add config option to add `CONTRIBUTING.md` to your project
  (via URL or file path).
* [#27](https://github.com/kowainik/summoner/issues/27):
  Remove `b` script.

1.0.6
=====

* Use `relude` instead of `universum`.
* [#105](https://github.com/kowainik/summoner/issues/105):
  Add `--ignore-config` option.

1.0.5
=====

* [#100](https://github.com/kowainik/summoner/issues/100):
  Bump up to `ghc-8.4.3`. Add support for `Ghc843` in code
  and make it default.
* Make CI badges in README be shown depending on the chosen options.
* [#99](https://github.com/kowainik/summoner/issues/99):
  UseTravis-specific env variable `TRAVIS_BUILD_DIR` in created travis file.
* [#97](https://github.com/kowainik/summoner/issues/97):
  Add cabal to created travis file.
* [#96](https://github.com/kowainik/summoner/issues/96):
  Add option to choose `cabal`, `stack` or both.

1.0.4
=====
* Bump up `tomland` to version `0.3`.

1.0.3
=====
* [#92](https://github.com/kowainik/summoner/issues/92):
  Remove -fhide-source-paths from generated stack yaml files for
  GHC < 8.2

1.0.2
=====

* [#87](https://github.com/kowainik/summoner/issues/87):
  Fix Travis matrix for not default `ghc` versions.

1.0.1
=====

* [#85](https://github.com/kowainik/summoner/issues/85):
  Remove `base-noprelude` from dependencies.

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
* [#62](https://github.com/kowainik/summoner/issues/62):
  Make some parts of output not only colorful, but also bold. This makes output prettier.
* [#67](https://github.com/kowainik/summoner/issues/67):
  Add `Aswer` data type to handle yes-no CLI questions.
* [#61](https://github.com/kowainik/summoner/issues/61):
  Add CLI `--version` option.
* [#73](https://github.com/kowainik/summoner/issues/73):
  Make custom prelude be table in toml.
* [#74](https://github.com/kowainik/summoner/issues/74):
  Print hierarchy tree for the created project.

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
