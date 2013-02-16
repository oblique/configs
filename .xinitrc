#!/bin/sh

cd ~

if [[ -d /etc/X11/xinit/xinitrc.d ]]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
        [[ -x "$f" ]] && . "$f"
    done
    unset f
fi

# nvidia flashplugin fix
export VDPAU_NVIDIA_NO_OVERLAY=1

[[ -f ~/.Xresources ]] && xrdb -load ~/.Xresources

setxkbmap -layout us,el
setxkbmap -variant ,extended
setxkbmap -option
setxkbmap -option grp:rctrl_rshift_toggle
setxkbmap -option terminate:ctrl_alt_bksp
setxkbmap -option compose:menu

# remap Menu key
xmodmap -e "keycode 135 = Menu"

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

# load nvidia settings
[[ -f ~/.nvidia-settings-rc ]] && nvidia-settings -l
nvidia-settings -a ':0[dpy:1]/Gamma=0.820'

# synaptics settings
synclient VertTwoFingerScroll=1
synclient TapButton1=0 TapButton2=0 TapButton3=0

# clipboard synchronizer
autocutsel -fork
autocutsel -selection PRIMARY -fork

#export GDK_USE_XFT=1    # for old gtk applications
#export QT_XFT=true      # for old qt applicatios

DEFAULT_SESSION="i3"

if [[ -n "$1" ]]; then
    x=$1
else
    x=$DEFAULT_SESSION
fi

case $x in
    kde)
        export XSESSION="KDE-4"
        exec startkde
        ;;
    gnome)
        #export OOO_FORCE_DESKTOP=gnome
        #export XDG_MENU_PREFIX=gnome-
        exec gnome-session
        ;;
    fluxbox)
        exec startfluxbox
        ;;
    awesome)
        xsetroot -cursor_name left_ptr
        exec awesome
        ;;
    i3)
        xsetroot -solid "#101010"
        exec i3
        ;;
    *)
        echo "Wrong session name!"
        ;;
esac
