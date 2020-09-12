#!/usr/bin/env bash
# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

set -euo pipefail

docker pull codercom/code-server:latest
docker build -f ./Dockerfile -t localcodeserver .
