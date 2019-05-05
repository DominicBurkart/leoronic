#!/usr/bin/env bash

if  ! docker inspect leoronic > /dev/null 2> /dev/null # docker inspect fails if the image DNE
then
    echo "Building container..."
    bash ./build_image.sh
    echo "Running container..."
fi

# the child needs access to the docker socket.
docker run -v /var/run/docker.sock:/var/run/docker.sock leoronic