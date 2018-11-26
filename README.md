# 🔮 Summoner

![wizard](https://user-images.githubusercontent.com/8126674/44388234-320aac00-a55a-11e8-879d-5dca68512031.png)
[![Build status](https://secure.travis-ci.org/kowainik/summoner.svg)](http://travis-ci.org/kowainik/summoner)
[![Hackage](https://img.shields.io/hackage/v/summoner.svg)](https://hackage.haskell.org/package/summoner)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/kowainik/summoner/blob/master/LICENSE)

> _So many of our dreams at first seem impossible, then they seem improbable, and
> then, when we summon the will, they soon become inevitable._
>
> Christopher Reeve

Summoner is a tool for scaffolding fully configured batteries-included production Haskell projects.

Do you want to create a library that is to be uploaded to the Hackage/Stackage, that builds with both Cabal and Stack and supports the latest three major GHC versions?
Or you are building a production application which uses a custom prelude and has CI with Travis Linux and AppVeyors Windows checks?
Summoner can help you to do all of that with minimal effort from you - it can even upload the project to GitHub if you want to!

By the way, Summoner operates as either CLI or TUI application, so you can choose what you're more comfortable with and install only the required one.

## Structure

* [Demo](#demo-)
    + [TUI demo](#tui-demo-)
    + [CLI demo](#cli-demo-)
    + [Scaffolded project structure](#scaffolded-project-structure-)
* [Features](#features-)
* [Get started](#get-started-)
    + [Prerequisites](#prerequisites-)
    + [Installation](#installation-)
        + [Summon-TUI](#summon-tui-)
          + [TUI: from source](#tui-from-source-)
        + [Summon-CLI](#summon-cli-)
          + [CLI: from Hackage/Stackage](#cli-from-hackagestackage-)
          + [CLI: from source](#cli-from-source-)
    + [Setup](#setup-)
* [Usage](#usage-)
    + [Command-line arguments](#command-line-arguments-)
    + [File configuration](#file-configuration-)
    + [TUI](#tui-)
    + [CLI](#cli-)
* [For Developers](#for-developers-)
    + [Build](#build-)
    + [Test](#test-)
    + [Run](#run-)
* [Changelog](#changelog-)
* [Acknowledgments](#acknowledgments-)

## Demo [↑](#structure)

### TUI demo [↑](#structure)

TODO: TUI demo asciinema

### CLI demo [↑](#structure)

[![asciicast](https://asciinema.org/a/198918.png)](https://asciinema.org/a/198918)

### Scaffolded project structure [↑](#structure)

This is the example of the project hierarchy you can get if you're running Summoner with all options enabled:

```
project-name/
├── app/
│   └── Main.hs
├── benchmark/
│   └── Main.hs
├── src/
│   ├── ProjectName.hs
│   └── Prelude.hs
├── test/
│   └── Spec.hs
├── CHANGELOG.md
├── CONTRIBUTING.md
├── LICENSE
├── project-name.cabal
├── README.md
├── Setup.hs
├── stack.yaml
├── appveyor.yml
├── .git
├── .gitignore
├── .stylish-haskell.yaml
└── .travis.yml
```

You can also see complete examples in the following folder:

* [`summon-cli/tests/golden`: Examples of scaffolded projects](https://github.com/kowainik/summoner/tree/master/summoner-cli/test/golden)

Directory `smallProject` contains an example of the straightforward package, and `fullProject` shows generated project with more advanced structure and more features.

## Features [↑](#structure)

Summoner is the tool that combines the predefined configurations, command-line arguments and chosen interface.

To fully understand the power in your hands with the help of Summoner, please, check out this section.

Below you can see highlighted features in different categories.

### Project [↑](#structure)

+ Cabal and Stack build tools support.
+ Ability to pick stanzas (library, executable, test-suite, benchmark).
+ Option to include an alternative prelude, if needed. The project then would use [`base-noprelude` technique](http://hackage.haskell.org/package/Prelude), and `Prelude` module would be added to the library target.
+ Whole Hackage-upload checklist support.
+ Support of the multiple GHC versions, with the thoughtful reflection on project meta, base versions (e.g. `base >= 4.9 && < 4.12`), etc.
+ Different license support: MIT, BSD2, BSD3, GPL-2, GPL-3, LGPL-2.1, LGPL-3, AGPL-3, Apache-2.0, MPL-2.0, None (All Rights Reserved license without file).
+ Creation of the `CHANGELOG.md` file with [PVP versioning policy](https://pvp.haskell.org).
+ Ability to include your `.stylish-haskell.yaml` file.
+ Usage of the `ghc-options` field with the sensible defaults.

  If warnings are not explicitly stated in the configuration file, then the following list of GHC flags is added to all stanzas:

  ```
  -Wall
  -Wincomplete-uni-patterns
  -Wincomplete-record-updates
  -Wcompat
  -Widentities
  -Wredundant-constraints      (ghc >= 8.0)
  -fhide-source-paths          (ghc >= 8.2.2)
  -Wmissing-export-lists       (ghc >= 8.4.1)
  -Wpartial-fields             (ghc >= 8.4.1)
  ```

  Besides, the following GHC options are added to the executable, tests and benchmark stanzas:

  ```
  -threaded
  -rtsopts
  -with-rtsopts=-N
  ```

### GitHub [↑](#structure)

+ Initialisation of the git repository inside the project folder.
+ Initial commit creation.
+ Uploading the repository to GitHub.
+ Exhaustive `.gitignore` file.
+ Formation of the `README` file with Hackage, Stackage and CI badges.
+ Linking to the GitHub repository in the `.cabal` file.
+ Ability to include your `CONTRIBUTING.md` file.

### CI [↑](#structure)

+ Generation of the `.travis.yml` file that runs build and tests on CI under Linux.
+ Configuration matrix on CI to build with multiple GHC versions and various build tools.
+ `-Werror` is enabled on CI not to miss any warnings.
+ Run HLint checks on CI.
+ Generation of the `appveyor.yaml` file which runs build and tests on CI under Windows.

### Others [↑](#structure)

+ Ability to create a project in the offline mode.

## Get started [↑](#structure)

### Prerequisites [↑](#structure)

To start using Summoner make sure that you have the following tools installed on your machine:

* [`git`](https://git-scm.com) – to initialize the GitHub repo.
* [`hub`](https://github.com/github/hub) – to upload the project to GitHub.
* [`curl`](https://curl.haxx.se) – to download licenses.

### Installation [↑](#structure)

Summon contains two executables:

* `summon`: scaffold projects using interactive CLI mode.
* `summon-tui`: scaffold projects using TUI.

Below you can find the description of how to install each of them so you can choose the one you want.

#### Summon-TUI [↑](#structure)

> **NOTE:** `summon-tui` is not supported on Windows. See [this issue](https://github.com/jtdaugherty/vty/pull/1).

##### TUI: from source [↑](#structure)

Fetch the repo using `cabal`:

```shell
cabal get -s summoner
```

or clone the directory from GitHub:

```shell
git clone https://github.com/kowainik/summoner.git
```

Step into the directory:

```haskell
cd summoner
```

Build and install using `cabal`:

```shell
cabal new-install summoner-tui:exe:summon-tui
```

Build and install using `stack`:

```shell
stack install summoner-tui:exe:summon-tui
```

#### Summon-CLI [↑](#structure)

##### CLI: from Hackage/Stackage [↑](#structure)

Using `cabal`:

```shell
cabal new-update
cabal new-install summoner
```

Using `stack`:

```shell
stack install summoner
```

##### CLI: from source [↑](#structure)

Fetch the repo using `cabal`:

```shell
cabal get -s summoner
```

or clone the directory from GitHub:

```shell
git clone https://github.com/kowainik/summoner.git
```

Step into the directory:

```haskell
cd summoner
```

Build and install using `cabal`:

```shell
cabal new-install summoner-cli:exe:summon
```

Build and install using `stack`:

```shell
stack install summoner:exe:summon
```

### Setup [↑](#structure)

You can turn on the bash auto-completion by running the following command:

```shell
source <(summon --bash-completion-script `which summon`)
```

## Usage [↑](#structure)

To start `summon`ing projects nothing additional is required. However, for the more customizable setting, you can use additional configuration settings.
There are several options how to set the particular configurations for the new projects (in priority increasing order):

1. Default configuration file (`~/.summoner.toml`).
2. Explicitly specified configuration file by `--file FILENAME` option (used instead of the default one if specified).
3. Options that are stated by CLI arguments.
4. User input. It could be
    * **TUI** – User data specified in the window of `summon-tui`.
    * **CLI** – Interactively inputted answers during execution of the `summon` command (for the options that were not specified on previous steps).

Cut a long story short, the configuration uses [Partial Options Monoid Pattern](https://medium.com/@jonathangfischoff/the-partial-options-monoid-pattern-31914a71fc67).

### File configuration [↑](#structure)

Here is the list of the options that could be configured for your needs. If options are not specified, they are asked interactively (or some sensible defaults are used).

| Field            | Type    | Description                                                                                                                                                          |
|------------------|---------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `cabal`          | Bool    | Add Cabal support?                                                                                                                                                   |
| `stack`          | Bool    | Add Stack support?                                                                                                                                                   |
| `owner`          | Text    | GitHub login.                                                                                                                                                        |
| `fullName`       | Text    | Full name.                                                                                                                                                           |
| `email`          | Text    | E-mail address.                                                                                                                                                      |
| `license`        | License | One of: `MIT`, `BSD2`, `BSD3`, `GPL-2`, `GPL-3`, `LGPL-2.1`, `LGPL-3`, `AGPL-3`, `Apache-2.0`, `MPL-2.0`, `None`.                                                    |
| `ghcVersions`    | [GHC]   | `summoner` uses default `GHC-8.4.4`. However, additionally you can specify other versions. For each version `x.y.z` the `stack-x.y.z.yaml` will be created.               |
| `github`         | Bool    | Turn on `GitHub` integration by default?                                                                                                                             |
| `private`        | Bool    | Create private repository by default? (Ignored if `github = false`)                                                                                                |
| `travis`         | Bool    | Turn on `Travis` integration by default?  (Ignored if `github = false`)                                                                                                                            |
| `appveyor`       | Bool    | Turn on `AppVeyor` integration by default?  (Ignored if `github = false`)                                                                                                                          |
| `lib`            | Bool    | Create `src` folder with simple `ProjectName.hs` file and library target?                                                                                            |
| `exe`            | Bool    | Create `app` folder with simple `Main.hs` file and executable target?                                                                                                |
| `test`           | Bool    | Create `test` folder with simple `Spec.hs` file and test target?                                                                                                     |
| `bench`          | Bool    | Create `benchmark` folder with `Main.hs` file with [`gauge`](https://hackage.haskell.org/package/gauge) library usage example?                                       |
| `extensions`     | [Text]  | List of the default extensions to add into `default-extensions` section in the `.cabal`.                                                                             |
| `warnings`       | [Text]  | List of the default checks and warnings to add into `ghc-options` section in the `.cabal`.                                                                           |
| `stylish.*`      | Text    | `stylish.file` to provide the absolute file path OR `stylish.url` to download the `.stylish-haskell.yaml` file to use in the project.                                |
| `contributing.*` | Text    | `contributing.file` to provide the absolute file path OR `contributing.url` download OR `contribuint.link` to link the `CONTRIBUTING.md` file to use in the project. |
|`[prelude]`       |         |                                                                                                                                                                      |
| `package`        | Text    | Name of the package of the custom prelude you'd like to use in the project (doesn't work without `module` field).                                                    |
| `module`         | Text    | Name of the module of the custom prelude you'd like to use in the project (doesn't work without `package` field).                                                    |

See the example of [the configuration for projects of the `Kowainik` organization](https://github.com/kowainik/org/blob/master/.summoner.toml).

### Command line arguments [↑](#structure)

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

#### **summon new** command: [↑](#structure)

```
Usage:
  summon new [PROJECT_NAME] [--ignore-config] [--no-upload] [--offline]
             [-f|--file FILENAME]
             [--cabal]
             [--stack]
             [--prelude-package PACKAGE_NAME]
             [--prelude-module MODULE_NAME]
             [with    [OPTIONS]]
             [without [OPTIONS]]

Available options:
  -h, --help               Show this help text
  --ignore-config          Ignore configuration file
  --no-upload              Do not upload to GitHub. Special case of the '--offline' flag.
  --offline                Offline mode: create project with 'All Rights Reserved' license
                           and without uploading to GitHub.
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

#### **summon show** command: [↑](#structure)

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

For example, the following command preconfigures the project settings with the custom prelude `relude`, included library, executable, test stanzas (but without benchmarks), creates a private repository on GitHub with the Travis CI and AppVeyor CI integrated.

```shell
summon new my-project with -letgcpw without -b --prelude-package relude --prelude-module Relude
```

### TUI [↑](#structure)

#### TUI new command [↑](#structure)

TODO: screenshot

The TUI window opens with the filled in information received from a config file and command-line arguments. However, it's possible to change any field you would like.

At the screenshot you can see that you'll be given a few text fields to fill in or some checkboxes to represent boolean values of the configurations. To help you fill in the correct input, there are some ⓘ info messages in the `Errors` section, which give additional details on the current field.

To help you navigate between the form fields here are the available hotkeys:

| Key                               | Action                                                                               |
|-----------------------------------|--------------------------------------------------------------------------------------|
| <kbd>Esc</kbd>                    | Abort `summoner`                                                                     |
| <kbd>Enter</kbd>                  | If input is valid go to the Confirm window                                           |
| <kbd>Tab</kbd>                    | Change the focus to the next active field                                            |
| <kbd>Shift</kbd> + <kbd>Tab</kbd> | Change the focus to the previous active field                                        |
| <kbd>Space</kbd>                  | Enable/disable current checkbox or radio button                                      |
| <kbd>Ctrl</kbd> + <kbd>d</kbd>    | Delete the input of the current text field                                           |
| <kbd>Ctrl</kbd> + <kbd>u</kbd>    | Delete the input of the current text field from the cursor position to the beginning |
| <kbd>Ctrl</kbd> + <kbd>k</kbd>    | Delete the input of the current text field from the cursor position to the end       |
| <kbd>Ctrl</kbd> + <kbd>a</kbd>    | Move the cursor to the beginning of the current input field                          |
| <kbd>Ctrl</kbd> + <kbd>e</kbd>    | Move the cursor to the end of the current input field                                |
| <kbd>Up</kbd>/<kbd>Down</kbd>     | Scroll license list when focused                                                     |

Note, that the form should be valid in order to be able to go to the Confirm window. If there are some errors in the input you could see the information about these errors in the `Error` section of the form.

#### TUI show command [↑](#structure)

These commands display the list of supported GHC versions or Licenses. Also, when the license name is specified the window with the scrollable content of the License text is shown.

### CLI [↑](#structure)

#### CLI new command [↑](#structure)

In CLI mode of operation Summoner asks about every project setting. Most of the questions contain default values, so you can press <kbd>Enter</kbd> to choose a default value.
If some option is specified via a configuration file or CLI arguments, then the question is skipped and the predefined value is used.

#### CLI show command [↑](#structure)

These commands display the list of the supported GHC versions, or Licenses. Also, when the license name is specified the content of the License is outputted to the terminal.

## For Developers [↑](#structure)

If you'd like to take part in the development processes here are few specialities to keep in mind:

* Summoner is the multi-package project which has two packages inside: `summoner-cli` and `summoner-tui`. We are supporting the same functionality in both of them, so, if it's possible, you should add/change the code in both of the packages.
* Summoner uses the custom prelude [`relude`](https://github.com/kowainik/relude).
* We are using [Kowainik style guide](https://github.com/kowainik/org/blob/master/style-guide.md#haskell-style-guide).
* To sum up, [here is the Contributing guide](https://github.com/kowainik/org/blob/master/CONTRIBUTING.md#contributing-to-the-kowainik-repositories) we use across the repositories.
* This project is contributors-friendly, so be kind to other people working on the project.

### Build [↑](#structure)

To build the project you can use the following commands:

```shell=
cabal new-build all
```

for Cabal and

```shell=
stack build
```

for Stack.

### Test [↑](#structure)

Summoner uses the golden tests technic for assuring the correctness of the generated files. For this purpose, we have [the folder](https://github.com/kowainik/summoner/tree/master/summoner-cli/test/golden) with two sample projects to test against.

To actually run tests you need to

```shell
cabal new-test all
```

or

```shell
stack test
```

### Run [↑](#structure)

Building Summoner with the suggested way creates two executables: `summon` and `summon-tui`. To run the compiled executable you can use the following commands:

```shell
cabal new-exec summon -- SOME_COMMAND
cabal new-exec summon-tui -- SOME_COMMAND
```

or

```shell
stack exec summon -- SOME_COMMAND
stack exec summon-tui -- SOME_COMMAND
```

## Changelog [↑](#structure)

Each package has it's own changelog:

* [Summoner changelog](https://github.com/kowainik/summoner/blob/master/summoner-cli/CHANGELOG.md#changelog).
* [Summoner-TUI changelog](https://github.com/kowainik/summoner/blob/master/summoner-tui/CHANGELOG.md#changelog).

## Acknowledgments [↑](#structure)

This project was inspired by [Aelve/new-hs](https://github.com/aelve/new-hs#readme),
which is the tool with the same goal but it's only for creating cabal projects.

Icons made by [Nikita Golubev](https://www.flaticon.com/authors/nikita-golubev)
from [Flaticon](https://www.flaticon.com/) is licensed by
[CC 3.0 BY](http://creativecommons.org/licenses/by/3.0/).
