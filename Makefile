#!/usr/bin/env make

# Copyright (C) 2024 The Quo-Emacs Authors
#
# This library is free software; you can redistribute it and/or modify it under
# the terms of the GNU Lesser General Public License as published by the Free
# Software Foundation; version 2.1.
#
# This library is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License along
# with this library; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

SHELL := /bin/bash

DEB_VER ?= $(shell \
	if [ -f /etc/debian_version ]; then \
		cat /etc/debian_version \
			| perl -pe 's!^\s*(\d+)(?:\.\d+)??\s*$$!$$1!'; \
	fi)

.PHONY: help
.PHONY: deps deps-debian deps-golang deps-nodejs deps-python
.PHONY: factory-reset reset-transient reset-tree-sitter reset-elpa-straight reset-startup-answers

help:
	@echo "usage: make <help>"
ifneq (${DEB_VER},)
	@echo "       make <deps|deps-debian|deps-golang|deps-nodejs|deps-python>"
else
	@echo "       make <deps|deps-golang|deps-nodejs|deps-python>"
endif
	@echo "       make <factory-reset>"
	@echo "       make <reset-startup-answers|reset-transient>"
	@echo "       make <reset-tree-sitter|reset-elpa-straight>"

#
#: Dependencies
#

ifneq (${DEB_VER},)
_DEBIAN_LIST += fd-find texinfo ccls shellcheck
ifeq (${DEB_VER},11)
_DEBIAN_LIST += python3-pyls python3-pyls-black python3-pyls-jsonrpc pylint
else
_DEBIAN_LIST += python3-pylsp python3-pylsp-black python3-pylsp-jsonrpc pylint
endif
deps-debian:
	sudo apt install ${_DEBIAN_LIST}
endif

deps-golang:
	@go install golang.org/x/tools/gopls@latest
	@go install golang.org/x/tools/cmd/godoc@latest
	@go install golang.org/x/tools/cmd/goimports@latest
	@go install github.com/rogpeppe/godef@latest
	@go install github.com/mdempsky/unconvert@latest
	@go install honnef.co/go/tools/cmd/staticcheck@latest
	@go install github.com/kisielk/errcheck@latest

deps-nodejs:
	@npm install -g \
		eslint \
		bash-ls \
		prettier \
		typescript \
		intelephense \
		bash-language-server \
		unified-language-server \
		typescript-language-server \
		vscode-langservers-extracted

deps-python:
	@pip3 install \
		git+https://github.com/psf/black \
		autotools-language-server

ifneq (${DEB_VER},)
deps: deps-debian deps-golang deps-nodejs deps-python
else
deps: deps-golang deps-nodejs deps-python
endif

#
#: Factory Reset
#

define _prompt_remove_path
TGT_NAME=$$(basename "$(1)"); \
if [ -d "$(1)" ]; then \
	read -n 1 -p "# remove path \"$${TGT_NAME}\"? (yN) " ANSWER; \
	case "$${ANSWER}" in \
		"y"|"Y") \
			echo ""; \
			if rm -rf "$(1)"; then \
				echo "removed path: $(1)"; \
			fi;; \
		"") ;; \
		*) echo "";; \
	esac; \
fi
endef

define _prompt_remove_file
TGT_NAME=$$(basename "$(1)"); \
if [ -f "$(1)" ]; then \
	read -n 1 -p "# remove file \"$${TGT_NAME}\"? (yN) " ANSWER; \
	case "$${ANSWER}" in \
		"y"|"Y") \
			echo ""; \
			if rm -f "$(1)"; then \
				echo "removed file: $(1)"; \
			fi;; \
		"") ;; \
		*) echo "";; \
	esac; \
fi
endef

factory-reset: reset-elpa reset-straight \
	reset-tree-sitter \
	reset-transient reset-startup-answers

reset-elpa:
	@$(call _prompt_remove_path,./elpa)

reset-straight:
	@$(call _prompt_remove_path,./straight)

reset-transient:
	@$(call _prompt_remove_path,./transient)

reset-tree-sitter:
	@$(call _prompt_remove_path,./tree-sitter)

reset-startup-answers:
	@$(call _prompt_remove_file,./transient/qemacs-startup-answers)
