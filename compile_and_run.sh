#!/usr/bin/env bash

cd "$(git rev-parse --show-toplevel)" || exit
( cd core && erlc +debug ./*.erl )
erl -sname leoronic -setcookie "$(cat .leoronic_cookie)" -pa core -s leoronic