#!/bin/sh
#
# to pass arguments with startx do:
# % startx ~/.xinitrc args...

DEFAULT_SESSION="i3"

cd ~

if [[ -d /etc/X11/xinit/xinitrc.d ]]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
        [[ -x "$f" ]] && . "$f"
    done
    unset f
fi

[[ -f ~/.Xresources ]] && xrdb -load ~/.Xresources

# keyboard settings
setxkbmap -layout us,el
setxkbmap -variant ,extended
setxkbmap -option
setxkbmap -option grp:rctrl_rshift_toggle
setxkbmap -option terminate:ctrl_alt_bksp
setxkbmap -option compose:menu

# remap Menu key
xmodmap -e "keycode 135 = Menu"

# set primary screen
xrandr --output LVDS-0 --primary

# clipboard synchronizer
autocutsel -fork
autocutsel -selection PRIMARY -fork

if [[ -n "$1" ]]; then
    x=$1
else
    x=$DEFAULT_SESSION
fi

case $x in
    gnome)
        exec gnome-session
        ;;
    fluxbox)
        exec startfluxbox
        ;;
    i3)
        xsetroot -solid "#101010"
        exec i3
        ;;
    *)
        echo "Wrong session name!"
        ;;
esac
