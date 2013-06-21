#!/bin/bash
if [[ ! -d ~/.i3/dev/git/i3 || ! -d ~/.i3/dev/git/i3status ]]; then
    echo "run ~/.i3/dev/mki3git.sh first."
    exit 1
fi

exec Xephyr +xinerama -screen 956x510 -origin 956x0 -screen 956x510 -ac -noreset :1
