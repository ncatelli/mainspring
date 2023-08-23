#!/usr/bin/env bash

if [[ $# -ne 1 ]]; then
    echo "Illegal number of parameters" >&2
	exit 1
else
    cargo set-version ${1}
fi