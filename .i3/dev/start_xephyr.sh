#!/bin/bash
if [[ ! -d ~/.i3/dev/git/i3 || ! -d ~/.i3/dev/git/i3status ]]; then
    echo "run ~/.i3/dev/mki3git.sh first."
    exit 1
fi

exec Xephyr :1 -ac -screen 956x540 -noreset
