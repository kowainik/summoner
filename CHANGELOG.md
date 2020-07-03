# Changelog

`summoner` uses [PVP Versioning][1].
The changelog is available [on GitHub][2].

## Unreleased

* [#476](https://github.com/kowainik/summoner/issues/476):
  Switch from `aeson` to `microaeson`.
* [#466](https://github.com/kowainik/summoner/issues/466):
  Add ` --test-show-details=direct` to `cabal test` on CI.
* [#444](https://github.com/kowainik/summoner/issues/444):
  Support GHC-8.10.1 in the generated projects.
  Use GHC-8.10.1 by default in the generated projects.
  Use `nightly-2020-06-29` for stack resolver.
* [#445](https://github.com/kowainik/summoner/issues/445):
  Add `-Wunused-packages` for GHC-8.10.1.

## 2.0.1.1 — May 29, 2020

* [#467](https://github.com/kowainik/summoner/issues/467):
  Allow `aeson-1.5.0.0`.
* [#469](https://github.com/kowainik/summoner/issues/469):
  __#TUI__ Allow `brick-0.54`.

## 2.0.1.0 — May 21, 2020

* [#443](https://github.com/kowainik/summoner/issues/443):
  Support GHC-8.10.1 for `Summoner` project.
* [#459](https://github.com/kowainik/summoner/issues/459):
  Bump up `tomland` version to `1.3.0.0`.
* [#460](https://github.com/kowainik/summoner/issues/460):
  Bump up `relude` version to `0.7.0.0`.
  Remove `Summoner.Template.Mempty` as `memptyIfFalse` is imported from
  `Relude`.
* [#455](https://github.com/kowainik/summoner/issues/455):
  Allow `validation-selective` version `0.1.0.0`.
* [#452](https://github.com/kowainik/summoner/issues/452):
  Bump up `colourista` version to `0.1.0.0`. Remove the
  `Summoner.Ansi` module.
* __#TUI__ Bump up `brick` upper bound to allow `0.53`.
* __#CLI__ Bump up `time` upper bound to allow `1.10`.

## 2.0.0.0 – Mar 28, 2020

* [#246](https://github.com/kowainik/summoner/issues/246):
  Put common fields into the `common-options` [common stanza](https://vrom911.github.io/blog/common-stanzas).
  (by [@vrom911](https://github.com/vrom911))
* [#351](https://github.com/kowainik/summoner/issues/351),
  [#395](https://github.com/kowainik/summoner/issues/395),
  [#398](https://github.com/kowainik/summoner/issues/398):
  __#CLI__ __#TUI__
  Add GitHub Actions CI check as an option for the generated
  project.

  * Add `-a` or `--actions` CLI option for GitHub actions.
  * Add `GitHub Actions` entry into interactive CLI mode.
  * Add `GitHub Actions` button into TUI.
  * Add `githubActions` to the TOML configs.

  (by [@patrickt](https://github.com/patrickt), [@vrom911](https://github.com/vrom911), [@chshersh](https://github.com/chshersh))
* [#401](https://github.com/kowainik/summoner/issues/401),
  [#429](https://github.com/kowainik/summoner/issues/429):
  Support GHC-8.8.3 in the project.
  Make GHC-8.8.3 default for the generated projects.
  Upgrade Stack LTS to `15.5`.
  (by [@vrom911](https://github.com/vrom911))
* [#418](https://github.com/kowainik/summoner/issues/418):
  Bump default `cabal` version to `2.4` in the generated project.
  (by [@vrom911](https://github.com/vrom911))
* [#114](https://github.com/kowainik/summoner/issues/114):
  __#CLI__ __#TUI__
  Implement non-interactive mode with `-n | --non-interacive` command-line option.
  (by [@vrom911](https://github.com/vrom911))
* [#70](https://github.com/kowainik/summoner/issues/70):
  __#CLI__ __#TUI__
  Implement `summon config` command. This command will generate the
  default TOML configuration file with helpful comments.
  (by [@chshersh](https://github.com/chshersh))
* [#361](https://github.com/kowainik/summoner/issues/361):
  Always put all default warnings in `ghc-options` inside common
  stanza under cabal conditionals on the `GHC` version. Now they look
  like this:

  ```haskell
  if impl(ghc >= 8.4)
    ghc-options:       -Wmissing-export-lists
                       -Wpartial-fields
  if impl(ghc >= 8.8)
    ghc-options:       -Wmissing-deriving-strategies
  ```

  (by [@chshersh](https://github.com/chshersh))
* [#345](https://github.com/kowainik/summoner/issues/345):
  Apply `mixins` approach for using alternative preludes instead of
  the `base-noprelude` trick. Now it looks like this in the `.cabal`
  file:

  ```haskell
  mixins:              base hiding (Prelude)
                     , relude (Relude as Prelude)
  ```
  (by [@chshersh](https://github.com/chshersh))
* [#251](https://github.com/kowainik/summoner/issues/251):
  Update LICENSE names for `.cabal` files in generated projects.
  Add `ISC` to the list of the accepted licenses.
  (by [@vrom911](https://github.com/vrom911))
* __#CLI__ __#TUI__
  Customise outputted LICENSE text for `summon show license LICENSE_NAME` command
  (by [@vrom911](https://github.com/vrom911))
* [#399](https://github.com/kowainik/summoner/issues/399):
  Add Haddock header to library module.

  ```haskell
  {- |
  Copyright: (c) 2020 FULL_NAME
  SPDX-License-Identifier: LICENSE_NAME
  Maintainer: NAME <email@email.com>

  Project description
  -}
  ```
  (by [@vrom911](https://github.com/vrom911))
* Improve `maintainer` field in the generated `.cabal` file.
  (by [@vrom911](https://github.com/vrom911))
* [#430](https://github.com/kowainik/summoner/issues/430):
  Instead of not including old GHCs for Stack check at Travis,
  those versions are added to the `allow_failures` section of matrix.
  (by [@vrom911](https://github.com/vrom911))

  _Note:_ For the Stack failure reasons see [this issue](https://github.com/commercialhaskell/stack/issues/4488).
* [#394](https://github.com/kowainik/summoner/issues/394):
  Do not crush when `hub` is not installed. Instead, print descriptive warning
  message.
  (by [@vrom911](https://github.com/vrom911))
* [#34](https://github.com/kowainik/summoner/issues/34):
  Print helpful error message when `hub` fails (for example, if the
  password is incorrect).
  (by [@chshersh](https://github.com/chshersh))
* [#281](https://github.com/kowainik/summoner/issues/281):
  Add more example projects and golden tests for various scaffolding
  configurations.
  (by [@chshersh](https://github.com/chshersh))
* [#375](https://github.com/kowainik/summoner/issues/375):
  Remove deprecated `stylish` and `contributing` fields in the configurations.
  Use `files` instead.
  (by [@vrom911](https://github.com/vrom911))
* Use `--silent` and `--fail` options with `curl` commands.
  (by [@chshersh](https://github.com/chshersh))
* Upgrade GHC-8.6.5 LTS to `14.27`.
  (by [@vrom911](https://github.com/vrom911))
* __#TUI__ Allow `brick-0.52`.
  (by [@vrom911](https://github.com/vrom911))
* [#363](https://github.com/kowainik/summoner/issues/363):
  Move from `generic-deriving` to `generic-data`.
  (by [@chshersh](https://github.com/chshersh))
* [#434](https://github.com/kowainik/summoner/issues/434):
  Move to `validation-selective`.
  (by [@vrom911](https://github.com/vrom911))
* Use `colourista` for pretty terminal formatting.
  (by [@chshersh](https://github.com/chshersh))

## 1.4.0.0 – Dec 25, 2019 🎅

* [#339](https://github.com/kowainik/summoner/issues/339):
  Support GHC `8.8.1` in generated projects.
  __Note:__ Stack projects will use `nightly` resolver as the stable one is not
  out at the moment of this issue is implemented.
  (by [@vrom911](https://github.com/vrom911))
* Add `-Wmissing-deriving-strategies` option to GHC version `8.8.1` and upper
  in the generated projects.
  (by [@vrom911](https://github.com/vrom911))
* [#314](https://github.com/kowainik/summoner/issues/314):
  __#CLI__ __#TUI__ Improve `summon(-tui) show ghc` output.
  (by [@gableh](https://github.com/gableh), [@vrom911](https://github.com/vrom911))
* [#316](https://github.com/kowainik/summoner/issues/316):
  Add logos to README bagdes in the generated projects.
  (by [@vrom911](https://github.com/vrom911))
* Generate project of version `0.0.0.0` instead of `0.0.0`.
  (by [@vrom911](https://github.com/vrom911))
* [#322](https://github.com/kowainik/summoner/issues/322):
  Run HLint check before project build in `.travis.yml` file for
  the generated projects.
  (by [@vrom911](https://github.com/vrom911))
* [#382](https://github.com/kowainik/summoner/issues/382):
  Improve error logging when fetching the license from GitHub while generating
  the project.
  (by [@chshersh](https://github.com/chshersh))
* [#255](https://github.com/kowainik/summoner/issues/255):
  __#CLI__ __#TUI__ Autofill prelude module name when package name is typed.
  (by [@yigitozkavci](https://github.com/yigitozkavci))
* [#354](https://github.com/kowainik/summoner/issues/354):
  __#CLI__ __#TUI__ Validate prelude package name and module name in config file, CLI, TUI.
  (by [@yigitozkavci](https://github.com/yigitozkavci), [@vrom911](https://github.com/vrom911))
* [#315](https://github.com/kowainik/summoner/issues/315):
  Remove `-fhide-source-paths` from generated `stack.yaml` file
  as it is implied by the newer version of Stack.
  (by [@bangng](https://github.com/chshersh), [@chshersh](https://github.com/chshersh))
* Add `stack.yaml.lock` to `.gitignore` file in the generated project.
  (by [@vrom911](https://github.com/vrom911))
* [#318](https://github.com/kowainik/summoner/issues/318):
  Support GHC `8.6.5` in generated projects.
  (by [@vrom911](https://github.com/vrom911))
* Remove support of GHC `8.6.4`, `8.6.3`, `8.4.3`. Leave only the latest
  versions of each major release for generated projects.
  (by [@vrom911](https://github.com/vrom911))
* [#333](https://github.com/kowainik/summoner/issues/333):
  Introduce `files` option in the TOML configuration which allows to specify
  custom files. Deprecate `stylish` and `contributing` options.
  (by [@chshersh](https://github.com/chshersh))

  _Migration guide:_ Instead of

  ```toml
  stylish.url = "some url"
  contributing.link = "some link"
  ```

  specify them like this:

  ```toml
  files =
      [ { path = ".stylish-haskell.yaml"
        , url  = "some url"
        }
      , { path = "CONTRIBUTING.md"
        , link = "some link"
        }
      ]
  ```
* [#374](https://github.com/kowainik/summoner/issues/374):
  Remove `warnings` field in the TOML configuration which was deprecated in the previous release.
  (by [@chshersh](https://github.com/chshersh))

  _Migration guide:_ Rename `warnings` field to `ghc-options` instead.
* [#367](https://github.com/kowainik/summoner/issues/367):
  Add `raw` type of custom extra files in the TOML configuration.
  Remove `link` type of file.
  (by [@chshersh](https://github.com/chshersh))

  _Migration guide:_ Replace `link` with `raw` and specify any custom text you
  want.
* Rename `file` config field of `source` to `local` in the TOML configuration.
  (by [@chshersh](https://github.com/chshersh))
* [#327](https://github.com/kowainik/summoner/issues/327):
  Better AppVeyor CI configuration for both `cabal` and `stack` in the generated
  project.
  (by [@chshersh](https://github.com/chshersh))
* [#253](https://github.com/kowainik/summoner/issues/253):
  __#TUI__ Fix new line in the Description field of the form.
  (by [@yigitozkavci](https://github.com/yigitozkavci))
* [#385](https://github.com/kowainik/summoner/issues/385):
  Add golden tests for `summon show ghc` command's output.
  (by [@vrom911](https://github.com/vrom911))
* [#384](https://github.com/kowainik/summoner/issues/384):
  Test golden project build's success.
  (by [@chshersh](https://github.com/chshersh))
* [#318](https://github.com/kowainik/summoner/issues/318):
  Build Summoner multipackage project with GHC `8.6.5`.
  (by [@vrom911](https://github.com/vrom911))
* [#338](https://github.com/kowainik/summoner/issues/338):
  Build Summoner multipackage project with GHC-8.8.1.
  (by [@vrom911](https://github.com/vrom911))
* [#360](https://github.com/kowainik/summoner/issues/360):
  Use mixins with `base` and `relude` in Summoner. Drop `base-noprelude` trick.
  (by [@vrom911](https://github.com/vrom911))
* [#329](https://github.com/kowainik/summoner/issues/329):
  Bump up to `optparse-applicative-0.15`, use custom `ParserPrefs`.
  (by [@vrom911](https://github.com/vrom911))
* [#328](https://github.com/kowainik/summoner/issues/328):
  Upgrade `tomland` bounds to allow `1.1.0.0`.
  (by [@vrom911](https://github.com/vrom911))
* [#373](https://github.com/kowainik/summoner/issues/373):
  Bump up to `tomland-1.2.1.0`.
  (by [@vrom911](https://github.com/vrom911))
* Bump up to `relude-0.6.0.0`.
  (by [@vrom911](https://github.com/vrom911))
* [#337](https://github.com/kowainik/summoner/issues/337):
  Allow `generic-deriving-1.13`.
  (by [@chshersh](https://github.com/chshersh))
* [#341](https://github.com/kowainik/summoner/issues/341):
  Allow `ansi-terminal-0.10`.
  (by [@mpilgrem](https://github.com/mpilgrem))
* [#323](https://github.com/kowainik/summoner/issues/323):
  Allow `tree-diff-0.1`.
  (by [@vrom911](https://github.com/vrom911))
* [#320](https://github.com/kowainik/summoner/issues/320):
  Allow `hedgehog-1.0`.
  (by [@vrom911](https://github.com/vrom911))

## 1.3.0.1 — Apr 10, 2019

* Include golden test files into `extra-source-files`.

## 1.3.0 (TUI: 0.1.0) — Apr 9, 2019

* [#285](https://github.com/kowainik/summoner/issues/285):
  __#CLI__ __#TUI__ Implement `summon script` command.
* [#151](https://github.com/kowainik/summoner/issues/151),
  [#295](https://github.com/kowainik/summoner/issues/295):
  Add support for GHC-8.6. Make GHC-8.6.4 default.
* [#269](https://github.com/kowainik/summoner/issues/269):
  __Important:__ Introduce `ghc-options` configuration parameter. Deprecate
  `warnings` field in the TOML configuration.

  _Migration guide:_ Please, rename `warnings` field if you use one, it will be
  removed in the very next release. Use `ghc-options` instead.
* [#303](https://github.com/kowainik/summoner/issues/303):
  Add option `noUpload` to the TOML configuration.
* __Important:__ Summoner doesn't add old GHC versions into Travis matrix for
  Stack anymore. See this Stack issue for reasoning:
      https://github.com/commercialhaskell/stack/issues/4488
* [#280](https://github.com/kowainik/summoner/issues/280):
  Support build with `cabal-install` on the AppVeyor CI.

  __#TUI__ AppVeyor CI checkbox is no longer disabled when only `cabal` build tool is
  selected.
* [#272](https://github.com/kowainik/summoner/issues/272):
  Simplify Travis config for Cabal in the generated project.
* [#275](https://github.com/kowainik/summoner/issues/275):
  Simplify Travis settings/installation process for Stack in the generated
  project.
* [#261](https://github.com/kowainik/summoner/issues/261):
  Guess author login, name and email from `.gitconfig`.
* [#268](https://github.com/kowainik/summoner/issues/268):
  __#CLI__ Simplify process of adding custom prelude in the interactive mode.
* [#282](https://github.com/kowainik/summoner/issues/282):
  Allow users to extend the `.gitignore` file.
* [#286](https://github.com/kowainik/summoner/issues/286),
  [#288](https://github.com/kowainik/summoner/issues/288),
  [#304](https://github.com/kowainik/summoner/issues/304):
  Bump up to `tomland-1.0.0`, `relude-0.5.0`, `shellmet-0.0.1`.
* Drop support for GHC-8.6.3 on CI. Support GHC-8.6.4 on CI.
* __#TUI__ Upgrade to `summoner-1.3.0`.
* [#297](https://github.com/kowainik/summoner/issues/297):
  __#TUI__ Strip project description.
* [#304](https://github.com/kowainik/summoner/issues/304):
  __#TUI__ Bump up to `brick-0.47`.

## 1.2.0 (TUI: 0.0.0) — Nov 30, 2018

* [#208](https://github.com/kowainik/summoner/issues/208):
  __#TUI__ Implement initial TUI for `summoner`.
* Make `cabal-version: 2.0` default in generated projects.
* [#11](https://github.com/kowainik/summoner/issues/11):
  Support offline mode.
* [#199](https://github.com/kowainik/summoner/issues/199):
  Add `None` license with no `LICENSE` file generation.
  Add licenses short descriptions text during the interactive mode.
  Patch `summon show license` command to show short description about
  each license.
* [#197](https://github.com/kowainik/summoner/issues/197)
  Build with `stack` lts-12.20, and support `GHC` 8.4.4 in
  project generation. Make GHC-8.4.4 the default one for generated project.
* [#37](https://github.com/kowainik/summoner/issues/37):
  Add HLint check on Travis CI in generatd projects.
* [#142](https://github.com/kowainik/summoner/issues/142):
  Add version bounds to `base` in the generated `.cabal` file.
* [#239](https://github.com/kowainik/summoner/issues/239):
  Make categories not mandatory.
* Use `defaultDescription` for project description if not specified.
* [#185](https://github.com/kowainik/summoner/issues/185):
  Add golden tests.
* [#184](https://github.com/kowainik/summoner/issues/184)
  Add `--no-upload` option.
* [#169](https://github.com/kowainik/summoner/issues/169):
  Make AppVeyor use the 64bits version of stack and build for 64 bits.
* [#154](https://github.com/kowainik/summoner/issues/154):
  Add `Link` constructor to `Source` data type.
* [#178](https://github.com/kowainik/summoner/issues/178):
  Remove unnecessary new lines and trailing spaces in most places.
* [#168](https://github.com/kowainik/summoner/issues/168):
  Upgrade `stack` `intall-cabal` option's version to `2.2.0.1` in
  the generated Travis file.
* [#195](https://github.com/kowainik/summoner/issues/195):
  Update prompt questions for CLI.
* Add more documentation to the functions.
* [#155](https://github.com/kowainik/summoner/issues/155):
  Bump up to `tomland-0.5.0`.
* [#202](https://github.com/kowainik/summoner/issues/202):
  Bump up to `relude-0.4.0`.
* Remove `tasty` dependencies from tests.
* Drop support for GHC-8.6.1 on CI. Support GHC-8.6.2 on CI.
* Drop support for GHC-8.4.3 on CI. Build with GHC-8.4.4 on CI.
* Use `defaultDescription` for project description if not specified.

## 1.1.0.1 – Sep 10, 2018

* Bump up `relude` version to `0.3.0`.
* [#146](https://github.com/kowainik/summoner/issues/146):
  Migrate to `base-noprelude`.
* Add -Werror to build option in generated .travis.yml

## 1.1.0

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

## 1.0.6

* Use `relude` instead of `universum`.
* [#105](https://github.com/kowainik/summoner/issues/105):
  Add `--ignore-config` option.

## 1.0.5

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

## 1.0.4

* Bump up `tomland` to version `0.3`.

## 1.0.3

* [#92](https://github.com/kowainik/summoner/issues/92):
  Remove -fhide-source-paths from generated stack yaml files for
  GHC < 8.2

## 1.0.2

* [#87](https://github.com/kowainik/summoner/issues/87):
  Fix Travis matrix for not default `ghc` versions.

## 1.0.1

* [#85](https://github.com/kowainik/summoner/issues/85):
  Remove `base-noprelude` from dependencies.

## 1.0.0

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

## 0.0.4

* Now works on windows systems as well
* Add powershell install script for Windows


## 0.0.3

* [#45](https://github.com/vrom911/hs-init/issues/45):
  Support AppVeyor CI for created projects.

## 0.0.2

* Use metavar for on-off commands.
* Upgrade `lts` to `10.3`, use `ghc 8.2.2` as default value.

## 0.0.1

* [#36](https://github.com/vrom911/hs-init/issues/36):
  Add option for creating private repositories — `-p | --private`.

[1]: https://pvp.haskell.org
[2]: https://github.com/kowainik/summoner/releases
