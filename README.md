# 🔮 Summoner

[![Build status](https://secure.travis-ci.org/kowainik/summoner.svg)](http://travis-ci.org/kowainik/summoner)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/kowainik/summoner/blob/master/LICENSE)
[![Hackage](https://img.shields.io/hackage/v/summoner.svg)](https://hackage.haskell.org/package/summoner)
[![Stackage LTS](http://stackage.org/package/summoner/badge/lts)](http://stackage.org/lts/package/summoner)
[![Stackage Nightly](http://stackage.org/package/summoner/badge/nightly)](http://stackage.org/nightly/package/summoner)

This is tool for creating completely configured production Haskell projects.

## Demo

[![asciicast](https://asciinema.org/a/PCt492vIY7ccuNw3qBJFM9q9G.png)](https://asciinema.org/a/PCt492vIY7ccuNw3qBJFM9q9G)

## Getting started

### Prerequisites

To start using it make sure you have next tools installed on your machine:

* [`Stack`](http://haskellstack.org) or [`cabal`](https://www.haskell.org/cabal/)
* [`git`](https://git-scm.com)
* [`hub`](https://github.com/github/hub)

### Installation

Installation process can be done with one simple command:

    $ cabal install summoner

or

    $ stack install summoner

You can turn on the bash auto-completion by running the following command:

```
$ source <(summon --bash-completion-script `which summon`)
```

After that you can call `summon` with required command line options, follow
the instructions that will appear, and a new project would be created in a subfolder
as well as a repository under your github account (if requested).

### Usage

There are several options how to set particular configurations:

1. Default configuration file (`~/.summoner.toml`).
2. Explicitly specified configuration file by `--file FILENAME` option (used instead of default one if specified).
3. Options that are stated by CLI arguments.
4. Interactively inputed answers during work of the `summon` command
  (for the options that were not specified on previous steps).

So the configuration uses [`Partial Options Monoid Pattern`](https://medium.com/@jonathangfischoff/the-partial-options-monoid-pattern-31914a71fc67).

If none of the mentioned above cases used then the configuration will be built interactively.

#### Configurations

##### `.toml` files:

Here is the list of the options that could be configured for your needs:

###### Global keys

* `cabal` – `true` if you want to build you project with `cabal`,
             `false` if you don't. If not specified it would be asked during each run of the `summoner`.
* `stack` – `true` if you want to build your project with `stack`,
             `false` if you don't. If not specified it would be asked during each run of the `summoner`.
* `owner` – `GitHub` login.
* `fullName` – full name.
* `email` – e-mail address.
* `license` – license (possible options: `MIT`, `BSD2`, `BSD3`, `GPL-2`, `GPL-3`,
              `LGPL-2.1`, `LGPL-3`, `AGPL-3`, `Apache-2.0`, `MPL-2.0`).
* `ghcVersions` – `summoner` uses default `GHC-8.2.2`. But additionally you can specify other versions.
                   For each version `x.y.z` the `stack-x.y.z.yaml` will be created.
* `github` – `true` if you want to turn on `GitHub` integration by default,
             `false` if you don't. If not specified it would be asked during each run of the `summoner`.
* `travis` – `true` if you want to turn on `Travis` integration by default,
             `false` if you don't. Ignored if `github = false`.
             If not specified it would be asked during each run of the `summoner`.
* `appveyor` – `true` if you want to turn on `AppVeyor` integration by default,
               `false` if you don't. Ignored if `github = false`.
               If not specified it would be asked during each run of the `summoner`.
* `private` – `true` if you want to create private repositories by default,
              `false` if you don't. Ignored if `github = false`.
              If not specified it would be asked during each run of the `summoner`.
* `bscript` – `true` if you want to include [build script](#build-script) by default,
              `false` if you don't. If not specified it would be asked during each run of the `summoner`.
* `lib` – `true` if you want to create `src` folder with dummy `Lib.hs` file and library target by default,
          `false` if you don't. If not specified it would be asked during each run of the `summoner`.
* `exe` – `true` if you want to create `app` folder with dummy `Main.hs` file and executable target by default,
          `false` if you don't. If not specified it would be asked during each run of the `summoner`.
* `test` – `true` if you want to create `test` folder with dummy `Spec.hs` file and test target by default,
          `false` if you don't. If not specified it would be asked during each run of the `summoner`.
* `bench` – `true` if you want to create `benchmark` folder  with `Main.hs` file with [`gauge`](https://hackage.haskell.org/package/gauge) library usage example by default,
          `false` if you don't. If not specified it would be asked during each run of the `summoner`.
* `extensions` – List of the default extensions to add into `default-extensions` section in the `.cabal`.
* `warnings` – List of the default checks and warnings to add into `ghc-options` section in the `.cabal`.

###### Custom prelude options

Should be specified inside `[prelude]` table.

* `package` – Name of the package of the custom prelude you'd like to use in the project (doesn't work without `module` field).
* `module` – Name of the module of the custom prelude you'd like to use in the project (doesn't work without `package` field).

###### Examples

See example of [configuration for projects of `Kowainik` organization](https://github.com/kowainik/org/blob/master/.summoner.toml).

By default the `summoner` will look for the configuration file (`.summoner.toml`) in home directory.

The other way to specify some particular `.toml` file is `summon PROJECTNAME --file FILEPATH` command.

##### CLI

See the basic usage syntax below (you can check it out with `summon --help` command):

```
summon PROJECT_NAME [--cabal] [--stack] [--ignore-config]
       [with [OPTIONS]] [without [OPTIONS]]
       [-f|--file FILENAME]  [--prelude-package PACKAGE_NAME]
       [--prelude-module MODULE_NAME]

Available global options:
  -h, --help               Show this help text
  -v, --version            Show summoner's version
  --ignore-config          Ignore configuration file
  --cabal                  Cabal support for the project
  --stack                  Stack support for the project
  -f, --file FILENAME      Path to the toml file with configurations. If not
                           specified '~/.summoner.toml' will be used if present
  --prelude-package PACKAGE_NAME
                           Name for the package of the custom prelude to use in
                           the project
  --prelude-module MODULE_NAME
                           Name for the module of the custom prelude to use in
                           the project

Available commands:
  with                     Specify options to enable
  without                  Specify options to disable

Available command options:
  -h,--help                Show this help text
  -g, --github             Github integration
  -p, --private            Create private GitHub repository
  -c, --travis             Travis CI integration
  -w, --app-veyor          AppVeyor CI integration
  -s, --script             Build script
  -l, --library            Library target
  -e, --exec               Executable target
  -t, --test               Tests
  -b, --benchmark          Benchmarks

```

The options to be enabled/disabled can be specified while running the command.
If any of applicable command options wasn't tagged as enabled/disabled then
the question will be asked during the work of the script.

For example,

```
  summon newProject with -letgcspw without -b --prelude-package relude --prelude-module Relude
```
will create fully functional project which uses custom prelude `relude`, contains
library, executable file, tests, [build script](#build-script)
and create private repository on [github](https://github.com)
integrated with `Travis-CI`, `AppVeyor-CI`, but benchmarks won't be attached to this one.

But when calling this command

```
  summon newProject
```

the tool will ask about every particular option, rather you'd like to have it
or not in your project.

### Note

This tool was tested with next settings:

    stack version 1.6.1
    git   version 2.11.0
    hub   version 2.2.9

## Features

If you're running the `summoner` with all options enabled a project with the following
hierarchy will be created:

```
project-name
├── app
│   └── Main.hs
├── benchmark
│   └── Main.hs
├── src
│   ├── ProjectName.hs
│   └── Prelude.hs
├── test
│   └── Spec.hs
├── CHANGELOG.md
├── LICENSE
├── project-name.cabal
├── README.md
├── Setup.hs
├── stack.yaml
├── appveyor.yml
├── .git
├── .gitignore
└── .travis.yml
```
and also repository with one commit at master will be added with enabled `Travis-CI` for that.

## GHC options 

The following warning checks are added by default to every stanza:

    -Wall
    -threaded
    -rtsopts
    -with-rtsopts=-N

If `--file` option is not used, then

the following ghc-options are added to executable, test-suite and benchmark stanza.

    -Wincomplete-uni-patterns
    -Wincomplete-record-updates
    -Wmissing-import-lists
    -Wcompat
    -Widentities
    -Wredundant-constraints      (ghc >= 8.0)
    -fhide-source-paths          (ghc >= 8.2.2)
    -Wmissing-export-lists       (ghc >= 8.4.1)
    -Wpartial-fields             (ghc >= 8.4.1)

and the following ghc-options are added to benchmark stanza.

    -02

### Build script

The `b` script builds the project in a way that is convenient for developers.
It passes the right flags into right places, builds the project with --fast,
tidies up and highlights error messages in GHC output.

#### Usage

```
  ./b                 build whole project with all targets
  ./b -c              do stack clean
  ./b -t              build and run tests
  ./b -b              build and run benchmarks
  ./b --nix           use nix to build package
```

## Change log

[List of changes](https://github.com/kowainik/summoner/blob/master/CHANGELOG.md).

## Acknowledgments

This project was inspired by [Aelve/new-hs](https://github.com/aelve/new-hs#readme),
which is the tool with the same goal but it's using
[`cabal`](https://www.haskell.org/cabal/) for creating projects.
