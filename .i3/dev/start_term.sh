#!/bin/bash
if [[ ! -d ~/.i3/dev/git/i3 || ! -d ~/.i3/dev/git/i3status ]]; then
    echo "run ~/.i3/dev/mki3git.sh first."
    exit 1
fi

urxvt -e bash -c 'cd ~/.i3/dev; DISPLAY=:1.0 tmux' &
