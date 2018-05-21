# Summoner

[![Build status](https://secure.travis-ci.org/kowainik/summoner.svg)](http://travis-ci.org/kowainik/summoner)
[![Windows build status](https://ci.appveyor.com/api/projects/status/github/kowainik/summoner?branch=master&svg=true)](https://ci.appveyor.com/project/kowainik/summoner)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/kowainik/summoner/blob/master/LICENSE)
[![Hackage](https://img.shields.io/hackage/v/summoner.svg)](https://hackage.haskell.org/package/summoner)

This is tool for creating completely configured production Haskell projects.
Consider that it's using [`Stack`](http://haskellstack.org) for
creating and setting up projects.

## Getting started

### Prerequisites

To start using it make sure you have next tools installed on your machine:
* [`Stack`](http://haskellstack.org)
* [`git`](https://git-scm.com)
* [`hub`](https://github.com/github/hub)

### Installation

Installation process can be done with one simple command:

    $ stack install summoner

You can turn on the bash auto-completion by running the following command:

```
$ source <(summon --bash-completion-script `which summon`)
```

After that you can call `summon` with required command line options, follow
the instructions that will appear, and a new project would be created in a subfolder
as well as a repository under your github account (if requested).

### Usage

See the basic usage syntax below:

```
summon PROJECT_NAME [with [OPTIONS]] [without [OPTIONS]]

Available global options:
  -h, --help               Show this help text

Available commands:
  with                     Specify options to enable
  without                  Specify options to disable

Available command options:
  -g, --github             Github integration
  -c, --travis             Travis CI integration
  -w, --app-veyor          AppVeyor CI integration
  -s, --script             Build script
  -l, --library            Library target
  -e, --exec               Executable target
  -t, --test               Tests
  -b, --benchmark          Benchmarks
  -p, --private            Create private GitHub repository
```

The options to be enabled/disabled can be specified while running the command.
If any of applicable command options wasn't tagged as enabled/disabled then
the question will be asked during the work of the script.

For example,

```
  summon newProject with -letgcspw without -b
```
will create fully functional project with library, executable file, tests,
[build script](#build-script) and create private repository on [github](https://github.com)
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
├── appveyor.yml
├── .git
├── .gitignore
└── .travis.yml
```
and also repository with one commit at master will be added with enabled `Travis-CI` for that.

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
which is the tool with the same goal but the difference is that it's using
[`cabal`](https://www.haskell.org/cabal/) for creating projects.
