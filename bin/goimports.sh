#!/bin/bash
PACKAGES=(
    github.com/go-corelibs
    github.com/go-curses
    github.com/go-enjin
    github.com/go-coreutils
)
exec goimports \
     -local $(IFS=, ; echo "${PACKAGES[*]}") \
     "$@"
