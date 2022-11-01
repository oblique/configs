#!/bin/sh
set -e

if [[ -d ~/.fonts ]]; then
    echo "Running fc-cache"
    fc-cache
fi
