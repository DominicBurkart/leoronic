#!/usr/bin/env bash

# install elvis
curl https://s3.amazonaws.com/rebar3/rebar3 --output rebar3
chmod +x rebar3

git clone https://github.com/inaka/elvis
(
cd elvis || exit 1
../rebar3 compile
../rebar3 escriptize
)

mv elvis/_build/default/bin/elvis ./.elvis