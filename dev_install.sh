#!/usr/bin/env bash

# install rebar (for elvis)
curl https://s3.amazonaws.com/rebar3/rebar3 --output rebar3
chmod +x rebar3

# install elvis
git clone https://github.com/inaka/elvis
(
    cd elvis || exit 1
    ../rebar3 compile
    ../rebar3 escriptize
    mv _build/default/bin/elvis ../.elvis
)

# set up dialyzer
dialyzer --build_plt --apps erts kernel stdlib mnesia