#!/bin/sh

mkdir -p ~/.i3/dev/git
cd ~/.i3/dev/git

if [[ ! -d i3 ]]; then
    git clone git://code.i3wm.org/i3
    cd i3
    git checkout next
    make
    cd ..
fi

if [[ ! -d i3status ]]; then
    git clone git://code.i3wm.org/i3status
    cd i3status
    make
fi

if [[ ! -d i3lock ]]; then
    git clone git://code.i3wm.org/i3lock
    cd i3lock
    make
fi
