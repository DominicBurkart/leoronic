# Leoronic
[![build status](https://travis-ci.org/DominicBurkart/leoronic.svg?branch=master)](https://travis-ci.org/DominicBurkart/leoronic)
[![static analysis: dialyzer](https://img.shields.io/badge/static%20analysis-dialyzer-42f4c5.svg)](https://github.com/erlang/otp/tree/master/lib/dialyzer)
[![code style: elvis](https://img.shields.io/badge/code%20style-elvis-blue.svg)](https://github.com/inaka/elvis)
[![code style: prettier](https://img.shields.io/badge/code_style-prettier-ff69b4.svg?style=flat-square)](https://github.com/prettier/prettier)
![open issues](https://img.shields.io/github/issues/dominicburkart/leoronic.svg)
![latest release](https://img.shields.io/github/release/dominicburkart/leoronic.svg)
![last commit](https://img.shields.io/github/last-commit/dominicburkart/leoronic.svg)

Simple distributed computing.

### Installation

#### Start Docker.

Install & start the latest version of [Docker](https://docs.docker.com/install/).

#### Set your key.

Create a file and fill it with a security key for Leoronic (any
long set of random characters will do). For example, you can open a
command line and run a command like:

```bash
echo "93XGHfdgsdmKKvDYJ4c8FHKHG7nwADFBHSFG43tngsdv0cbverhrqHRTJcwDFBCTTYDvlrDGSHT4HBnpSTRHScv" >
your_leoronic_key_file
```

Two computers will need to have the same key in order to work together using leoronic.

### Install & Run

Run `docker pull dominicburkart/leoronic` to download the leoronic
docker image. Then, run `docker run -v /var/run/docker.sock:/var/run/docker.sock -v [path/to/your_leoronic_key_file]:/.leoronic_cookie leoronic`
to start leoronic, substituting `[path/to/your_leoronic_key_file]` for the path
the key file you've made.

## Contributing

Help or feedback is welcome and appreciated. Feel free to
[open an issue](https://github.com/DominicBurkart/leoronic/issues/new),
or if you have the technical expertise and time to contribute,
a dev guide is below.

### Current Dev Goals

- allow commands to be run outside of docker.
- allow commands to be passed to a waiting server program, which
  must be defined by the leoronic API for that language, and which
  must have a consistent (as of yet undefined) interface.
- package leoronic as a standalone binary (without docker).
- add integration tests.
- add unit tests for head selection behaviors.
- provide a GUI for leoronic.

### Dev Installation

To set up your dev environment,
make sure that you have both [Docker](https://docs.docker.com/install/)
and Erlang (available in every major system package manager)
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
