#!/usr/bin/env bash

if  ! docker inspect leoronic > /dev/null 2> /dev/null # docker inspect fails if the image DNE
then
    echo "Building container..."
    bash ./build_image.sh
    echo "Running container..."
fi

# pass the docker socket into the container so that any children container are actually siblings.
docker run -v /var/run/docker.sock:/var/run/docker.sock -v "$(pwd)"/.leoronic_cookie:/.leoronic_cookie leoronic