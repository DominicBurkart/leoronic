#!/usr/bin/env bash

bash ./dev_install.sh

# install pre-commit
curl https://pre-commit.com/install-local.py | python -
pre-commit install
pre-commit autoupdate
