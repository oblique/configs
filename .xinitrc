#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

[ -f .Xresources ] && xrdb -merge ~/.Xresources

setxkbmap -layout us,el
setxkbmap -option
setxkbmap -option 'grp:rctrl_rshift_toggle'
setxkbmap -option terminate:ctrl_alt_bksp

xinput list | sed -ne 's/^[^ ][^V].*id=\([0-9]*\).*/\1/p' | while read id
do
        case `xinput list-props $id` in
        *"Middle Button Emulation"*)
                xinput set-int-prop $id "Evdev Wheel Emulation" 8 1
                xinput set-int-prop $id "Evdev Wheel Emulation Button" 8 2
                xinput set-int-prop $id "Evdev Wheel Emulation Timeout" 8 200
                xinput set-int-prop $id "Evdev Wheel Emulation Axes" 8 6 7 4 5
                xinput set-int-prop $id "Evdev Middle Button Emulation" 8 0
                ;;
        esac
done

synclient VertTwoFingerScroll=1

# clipboard synchronizer
autocutsel -fork
autocutsel -selection PRIMARY -fork

#export GDK_USE_XFT=1    # for old gtk applications
#export QT_XFT=true      # for old qt applicatios

DEFAULT_SESSION="awesome"

if [ "$1" ]; then
    x=$1
else
    x=$DEFAULT_SESSION
fi

case $x in
    kde)
        export XSESSION="KDE-4"
        exec ck-launch-session startkde
        ;;
    gnome)
        #export OOO_FORCE_DESKTOP=gnome
        #export XDG_MENU_PREFIX=gnome-
        exec ck-launch-session gnome-session
        ;;
    fluxbox)
        exec ck-launch-session startfluxbox
        ;;
    awesome)
        xsetroot -cursor_name left_ptr
        exec ck-launch-session awesome
        ;;
    i3)
        exec ck-launch-session i3
        ;;
    pekwm)
        exec ck-launch-session pekwm
        ;;
    *)
        echo "Wrong session name!"
        ;;
esac
