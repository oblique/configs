#!/bin/bash
XEPHYR=$(which Xephyr)
cd ~
urxvt -e bash -c 'DISPLAY=:1.0 tmux' &
xinit ~/.i3/dev/i3.xinitrc -- ${XEPHYR} :1 -ac -screen 956x540 -reset -terminate
