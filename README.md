# hs-init

[![Build status](https://secure.travis-ci.org/vrom911/hs-init.svg)](http://travis-ci.org/vrom911/hs-init)

Original idea is taken from [Aelve/new-hs](https://github.com/aelve/new-hs).
The main difference is that this script is for creating and setting up project using [Stack](http://haskellstack.org).

## Getting started

### Prerequisites

* [Stack](http://haskellstack.org) 
* [git](https://git-scm.com)
* [hub](https://github.com/github/hub)
 
### Installation
Installation process can be done with one simple command:

    $ curl https://raw.githubusercontent.com/vrom911/hs-init/master/install | sh

This will download `hs-init.hs` and then put it in `/.local/bin`.

After that you can call `hs-init`, follow the instructions that will appear, and a new project would be created in a subfolder as well as a github repository.

### Note
This tool was tested with next settings:

    stack version 1.4.0
    git   version 2.9.3
    hub   version 2.2.9
## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE) file for details

## Acknowledgments

This project was inspired by [Aelve/new-hs](https://github.com/aelve/new-hs#readme).
