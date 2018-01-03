# hs-init

[![Build status](https://secure.travis-ci.org/vrom911/hs-init.svg)](http://travis-ci.org/vrom911/hs-init) [![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/vrom911/hs-init/blob/master/LICENSE) [![Version 1.0.0](https://img.shields.io/badge/version-v1.0.1-fabfff.svg)](https://github.com/vrom911/hs-init/blob/master/CHANGELOG.md)

This is tool for creating completely configured production Haskell projects.
Consider that this script is using [`Stack`](http://haskellstack.org) for creating and setting up projects.

## Getting started

### Prerequisites

To start using it make sure you have next tools installed on your machine:
* [`Stack`](http://haskellstack.org)
* [`git`](https://git-scm.com)
* [`hub`](https://github.com/github/hub)

### Installation
Installation process can be done with one simple command:

    $ curl https://raw.githubusercontent.com/vrom911/hs-init/master/install | sh

During this process you will be asked to input some data that will be used as default data for your further usages of `hs-init`. But don't worry, this is not permanent decision, the tool makes sure that you would like to use default data or you can input anything you'd like for particular projects you create.

The data you could input:

`Default Github username` — GitHub username

`Default name` — full name

`Default email address` — email address


If you won't fill in this fields the valid default values will be used instead.

After this will download `hs-init.hs` with default values corresponding to your answers and then put the executable in `~/.local/bin` (creates this folder if it doesn't exist).

After that you can call `hs-init` with required command line options, follow the instructions that will appear, and a new project would be created in a subfolder as well as a repository under your github account.

### Usage

See the basic usage syntax below:
```
hs-init PROJECT_NAME [COMMAND [COMMAND_OPTIONS]] [COMMAND [COMMAND_OPTIONS]]

Available global options:
  -h, --help               Show this help text

Available commands:
  on                       Specify options to enable
  off                      Specify options to disable

Available command options:
  -g, --github             Github integration
  -c, --ci                 CI integration (Travis CI)
  -s, --script             Build script
  -l, --library            Library target
  -e, --exec               Executable target
  -t, --test               Tests
  -b, --benchmark          Benchmarks
```
The options to be enabled/disabled can be specified while running the command. If any of applicable command options wasn't tagged as enabled/disabled then the question will be asked during the work of the script.

For example,
```
  hs-init newProject on -letgcs off -b
```
will create fully functional project with library, executable file, tests, [build script](#build-script) and create the repository on [github](https://github.com) integrated with `Travis-CI`, but benchmarks won't be attached to this one.

But when calling this command
```
  hs-init newProject
```
the tool will ask about every particular option, rather you'd like to have it or not in your project.

### Note
This tool was tested with next settings:

    stack version 1.4.0
    git   version 2.9.3
    hub   version 2.2.9

## Features

If you're running the `hs-init` with all options enabled a project with the following hierarchy will be created:

```
PROJECT_NAME
├── app
│   └── Main.hs
├── benchmark
│   └── Main.hs
├── src
│   └── Lib.hs
├── test
│   └── Spec.hs
├── CHANGELOG.md
├── LICENSE
├── PROJECT_NAME.cabal
├── README.md
├── Setup.hs
├── stack.yaml
├── .git
├── .gitignore
└── .travis.yml
```
and also repository with one commit at master will be added with enabled `Travis-CI` for that.

### Build script

The `b` script builds the project in a way that is convenient for developers. It passes the right flags into right places, builds the project with --fast, tidies up and highlights error messages in GHC output.

#### Usage

```
  ./b                 build whole project with all targets
  ./b -c              do stack clean
  ./b -t              build and run tests
  ./b -b              build and run benchmarks
  ./b --nix           use nix to build package
```

## Acknowledgments

This project was inspired by [Aelve/new-hs](https://github.com/aelve/new-hs#readme), which is the tool with the same goal but the difference is that it's using [`cabal`](https://www.haskell.org/cabal/) for creating projects.
