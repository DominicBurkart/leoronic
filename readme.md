# Leoronic

[![build status](https://travis-ci.org/DominicBurkart/leoronic.svg?branch=master)](https://travis-ci.org/DominicBurkart/leoronic)
[![code coverage](https://codecov.io/gh/DominicBurkart/leoronic/branch/master/graph/badge.svg)](https://codecov.io/gh/DominicBurkart/leoronic)
[![static analysis: dialyzer](https://img.shields.io/badge/static%20analysis-dialyzer-42f4c5.svg)](https://github.com/erlang/otp/tree/master/lib/dialyzer)
[![static analysis: mypy](http://www.mypy-lang.org/static/mypy_badge.svg)](http://mypy-lang.org/)
[![code style: elvis](https://img.shields.io/badge/code%20style-elvis-blue.svg)](https://github.com/inaka/elvis)
[![code style: prettier](https://img.shields.io/badge/code_style-prettier-ff69b4.svg?style=flat-square)](https://github.com/prettier/prettier)
[![code style: black](https://img.shields.io/badge/code%20style-black-000000.svg)](https://github.com/python/black)
![open issues](https://img.shields.io/github/issues/dominicburkart/leoronic.svg)
![latest release](https://img.shields.io/github/release/dominicburkart/leoronic.svg)
![last commit](https://img.shields.io/github/last-commit/dominicburkart/leoronic.svg)

Simple distributed computing.

### note: leoronic is a prototype to understand grid computing.

Leoronic is a distributed computing library. Acting as a thin (~1500 lines)
layer over OTP, it provides the same interface as Python's
multiprocessing library and requires minimal setup. Leoronic
automatically handles the loss or addition of new nodes to your
cluster, allowing you to increase throughput or handle hardware
failure on the fly, without stopping your applicaton
or modifying your code.

### System requirements

- Erlang/OTP >= 20
- bash
- perl

### Current Dev Goals

#### Completeness

- fix broken codecov analysis.
- Terminate jobs that exceed their allotted system resources
- Handle kwargs in python
- Create integration tests for task requirement setting
- Create unit tests for task requirement setting in python
- add tests for ability to correctly throw custom python errors

#### New Features

- fully support linux.
- allow commands to be run outside of docker.
- provide support for Rust and R.
- allow commands to be passed to a waiting server program, which
  must be defined by the leoronic API for that language, and which
  must have a consistent (as of yet undefined) interface.
- package leoronic as a standalone binary (without docker).
- provide a GUI for leoronic.

### Dev Installation

To set up your dev environment, make sure that you have Erlang (available in every major system package manager)
installed and updated. Then, download the leoronic repo and install
the git hooks as such:

```bash
git clone https://github.com/DominicBurkart/leoronic.git &&
cd leoronic &&
bash dev_install_contributer.sh
```

Finally, you'll also need to install [shellcheck](https://github.com/koalaman/shellcheck#installing).

Happy coding!

### Versioning

The version for the application is declared in `ebin/Leoronic.app`
(look for `vsn`).
