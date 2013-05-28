#!/bin/bash
if [[ ! -d ~/.i3/dev/git/i3 || ! -d ~/.i3/dev/git/i3status ]]; then
    echo "run ~/.i3/dev/mki3git.sh first."
    exit 1
fi

~/.i3/bin/i3-exec-wait.pl ~/.i3/dev/start_xephyr.sh
~/.i3/bin/i3-exec-wait.pl ~/.i3/dev/start_term.sh
~/.i3/bin/i3-exec-wait.pl ~/.i3/dev/start_i3.sh
