---
title: make
layout: notes
---

# example
```
.ONESHELL:
.SHELL := /usr/bin/env bash
.PHONY: configure format
BOLD=$(shell tput bold)
RED=$(shell tput setaf 1)
GREEN=$(shell tput setaf 2)
YELLOW=$(shell tput setaf 3)
RESET=$(shell tput sgr0)

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

configure: ## Configures and bootstraps the environment.
	@./configure

format: ## Applys terraform formatting and updates all copyright headers.
	@./bin/terraform-fmt
	@./bin/copyright-headers update
```
