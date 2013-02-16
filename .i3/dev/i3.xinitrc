#!/bin/sh
cd ~
[[ -f ~/.Xresources ]] && xrdb -load ~/.Xresources
xsetroot -solid "#606060"
exec i3 -c ~/.i3/dev/config
