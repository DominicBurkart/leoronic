#!/usr/bin/env bash
v=$(grep vsn ebin/Leoronic.app | cut -d '"' -f 2)
docker build -t leoronic:"$v" .
docker tag leoronic:"$v" leoronic:latest
docker tag leoronic:latest dominicburkart/leoronic:latest
docker tag leoronic:latest dominicburkart/leoronic:"$v"