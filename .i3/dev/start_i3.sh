#!/bin/bash
if [[ ! -d ~/.i3/dev/git/i3 || ! -d ~/.i3/dev/git/i3status ]]; then
    echo "run ~/.i3/dev/mki3git.sh first."
    exit 1
fi

[[ DISPLAY=:0* ]] && DISPLAY=:1
cd ~
[[ -f ~/.Xresources ]] && xrdb -load ~/.Xresources
xsetroot -solid "#606060"
exec ~/.i3/dev/git/i3/i3 -c ~/.i3/dev/git/i3/i3.config --force-xinerama
