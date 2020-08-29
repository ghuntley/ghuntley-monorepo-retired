#!/usr/bin/env bash
# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

#
set -euo pipefail

./build.sh

# shellcheck disable=SC2155,SC2006,SC1083
export PID=`docker ps |grep code- | awk {'print $1'}`
docker stop "${PID}"
docker rm "${PID}"

# shellcheck disable=SC2155,SC2006,SC1083
export CONTAINER_ID=`docker run --privileged=true -d --restart always -it -p 127.0.0.1:8443:8443 -v /var/run/docker.sock:/var/run/docker.sock -v "/home/ghuntley/.gitconfig:/home/coder/.gitconfig" -v "/home/ghuntley/code/:/home/coder/code" localcodeserver --allow-http  --no-auth --disable-telemetry`
docker cp ~/.ssh "${CONTAINER_ID}:/home/coder"
docker ps
