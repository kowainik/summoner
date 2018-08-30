# 🔮 Summoner

![wizard](https://user-images.githubusercontent.com/8126674/44388234-320aac00-a55a-11e8-879d-5dca68512031.png)
[![Build status](https://secure.travis-ci.org/kowainik/summoner.svg)](http://travis-ci.org/kowainik/summoner)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/kowainik/summoner/blob/master/LICENSE)
[![Hackage](https://img.shields.io/hackage/v/summoner.svg)](https://hackage.haskell.org/package/summoner)
[![Stackage LTS](http://stackage.org/package/summoner/badge/lts)](http://stackage.org/lts/package/summoner)
[![Stackage Nightly](http://stackage.org/package/summoner/badge/nightly)](http://stackage.org/nightly/package/summoner)

> _So many of our dreams at first seem impossible, then they seem improbable, and
> then, when we summon the will, they soon become inevitable._
>
> Christopher Reeve

Summoner is the tool for creating fully configured production Haskell projects.

## Demo

[![asciicast](https://asciinema.org/a/198918.png)](https://asciinema.org/a/198918)

## Getting started

### Prerequisites

To start using it make sure you have the next tools installed on your machine:

* [`Stack`](http://haskellstack.org) or [`cabal`](https://www.haskell.org/cabal/)
* [`git`](https://git-scm.com)
* [`hub`](https://github.com/github/hub)
* [`curl`](https://curl.haxx.se)

### Installation

The installation process can be done with the one simple command:

    $ cabal new-install summoner

or

    $ stack install summoner

or directly from GitHub.

You can turn on the bash auto-completion by running the following command:

```
$ source <(summon --bash-completion-script `which summon`)
```

After that, you can call `summon` with the required command. To create a
project, use `summon new` command specifying the prefered CLI options, follow
the instructions during the interactive process of the project creation, and a
new project would be created in a subfolder as well as a repository under your
GitHub account (if requested).

### Usage

There are several options how to set particular configurations for the new projects:

1. Default configuration file (`~/.summoner.toml`).
2. Explicitly specified configuration file by `--file FILENAME` option (used
   instead of the default one if specified).
3. Options that are stated by CLI arguments.
4. Interactively inputed answers during work of the `summon` command
  (for the options that were not specified on previous steps).

So, the configuration uses [`Partial Options Monoid Pattern`](https://medium.com/@jonathangfischoff/the-partial-options-monoid-pattern-31914a71fc67).

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
* `lib` – `true` if you want to create `src` folder with dummy `Lib.hs` file and library target by default,
          `false` if you don't. If not specified it would be asked during each run of the `summoner`.
* `exe` – `true` if you want to create `app` folder with dummy `Main.hs` file and executable target by default,
          `false` if you don't. If not specified it would be asked during each run of the `summoner`.
* `test` – `true` if you want to create `test` folder with dummy `Spec.hs` file and test target by default,
          `false` if you don't. If not specified it would be asked during each run of the `summoner`.
* `bench` – `true` if you want to create `benchmark` folder  with `Main.hs` file with [`gauge`](https://hackage.haskell.org/package/gauge)
            library usage example by default,
          `false` if you don't. If not specified it would be asked during each run of the `summoner`.
* `extensions` – List of the default extensions to add into `default-extensions` section in the `.cabal`.
* `warnings` – List of the default checks and warnings to add into `ghc-options` section in the `.cabal`.
* `stylish.*` — `stylish.file` to provide the absolute file path to the
  `.stylish-haskell.yaml` file to use in the project. `stylish.url` to provide
  the link to the `.stylish-haskell.yaml` file to use in the project. In case of
  the absense or wrong path/link no `.stylish-haskell.yaml` file is created.
* `contributing.*` — `contributing.file` to provide the absolute file path to the
  `CONTRIBUTING.md` file to use in the project. `contributing.url` to provide
  the link to the `CONTRIBUTING.md` file to use in the project. In case of
  the absense or wrong path/link no `CONTRIBUTING` file is created.

###### Custom prelude options

Should be specified inside `[prelude]` table.

* `package` – Name of the package of the custom prelude you'd like to use in the project (doesn't work without `module` field).
* `module` – Name of the module of the custom prelude you'd like to use in the project (doesn't work without `package` field).

###### Examples

See an example of [the configuration for projects of the `Kowainik` organization](https://github.com/kowainik/org/blob/master/.summoner.toml).

By default, the `summoner` looks for the configuration file (`.summoner.toml`) in home directory.

The other way to specify some particular `.toml` file is `summon new PROJECTNAME --file FILEPATH` command.

##### CLI

Available commands:

```
Usage:
  summon COMMAND
      Set up your own Haskell project

Available commands:
  new                      Create a new Haskell project
  show                     Show available licenses or ghc versions

Available global options:
  -h, --help               Show this help text
  -v, --version            Show summoner's version
```

**`summon new`** command:

```
Usage:
  summon new PROJECT_NAME [--cabal] [--stack] [--ignore-config]
             [with [OPTIONS]] [without [OPTIONS]]
             [-f|--file FILENAME]
             [--prelude-package PACKAGE_NAME]
             [--prelude-module MODULE_NAME]

Available options:
  -h, --help               Show this help text
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
  -h, --help               Show this help text
  -g, --github             Github integration
  -p, --private            Create private GitHub repository
  -c, --travis             Travis CI integration
  -w, --app-veyor          AppVeyor CI integration
  -l, --library            Library target
  -e, --exec               Executable target
  -t, --test               Tests
  -b, --benchmark          Benchmarks
```

**`summon show`** command:

```
Usage:
  summon show COMMAND
      Show supported licenses or ghc versions

Available commands:
  ghc                      Show available ghc versions
  license                  Show available licenses
  license [LICENSE_NAME]   Show specific license text

Available options:
  -h, --help               Show this help text
```

The options to be enabled/disabled can be specified while running the command.
If any of the applicable command options wasn't tagged as enabled/disabled, then
the question is asked during the work of the tool.

For example,

```
  summon new my-project with -letgcpw without -b --prelude-package relude --prelude-module Relude
```

creates the fully functional project which uses custom prelude `relude`, contains
library, executable file, tests and create private repository on [github](https://github.com)
integrated with `Travis-CI`, `AppVeyor-CI`, but benchmarks won't be attached to this one.

But when calling this command

```
  summon new my-project
```

the tool asks about every particular option, rather you'd like to have it
or not in your project.

## Features

If you're running the `summoner` with all options enabled a project with the following
hierarchy is created:

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

Moreover, a repository with one commit at master is added with enabled Travis CI for that.

## GHC options

The `-Wall` option is added to every stanza.

The following warning checks are added by default to executable, tests and benchmark stanzas:

    -threaded
    -rtsopts
    -with-rtsopts=-N

If warnings are not explicitly stated in the configuration file, then the
following ghc-options are added to all stanzas.

    -Wincomplete-uni-patterns
    -Wincomplete-record-updates
    -Wcompat
    -Widentities
    -Wredundant-constraints      (ghc >= 8.0)
    -fhide-source-paths          (ghc >= 8.2.2)
    -Wmissing-export-lists       (ghc >= 8.4.1)
    -Wpartial-fields             (ghc >= 8.4.1)

## Change log

[List of changes](CHANGELOG.md).

## Acknowledgments

This project was inspired by [Aelve/new-hs](https://github.com/aelve/new-hs#readme),
which is the tool with the same goal but it's only for creating cabal projects.

Icons made by [Nikita Golubev](https://www.flaticon.com/authors/nikita-golubev)
from [Flaticon](https://www.flaticon.com/) is licensed by
[CC 3.0 BY](http://creativecommons.org/licenses/by/3.0/).
