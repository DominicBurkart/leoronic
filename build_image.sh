#!/usr/bin/env bash
v=$(grep vsn ebin/Leoronic.app | cut -d '"' -f 2)
docker build -t leoronic:"$v" .
docker tag leoronic:"$v" leoronic:latest