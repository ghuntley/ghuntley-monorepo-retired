#!/usr/bin/env bash
# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

set -ueo pipefail

docker run \
    -p 9090:9090 \
    --name vouch-proxy \
    -v "${PWD}"/config:/config \
    -v "${PWD}"/data:/data \
    voucher/vouch-proxy


