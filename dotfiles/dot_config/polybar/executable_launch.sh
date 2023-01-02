#!/bin/bash

(
    flock -e 200

    # Kill current polybars and wait for them to exit
    while pgrep -u $UID -x polybar > /dev/null; do
        killall -q polybar
        sleep 0.5
    done

    # Find wlan interface
    for wlan in /sys/class/net/wl* /sys/class/net/wlan*; do
        if [[ -e "$wlan/wireless" ]]; then
            export WLAN_IFACE="${wlan##*/}"
            break
        fi
    done

    # Find eth interface
    for eth in /sys/class/net/en* /sys/class/net/eth*; do
        if [[ -e "$eth/device" ]]; then
            export ETH_IFACE="${eth##*/}"
            break
        fi
    done

    # Adjust sizes based on DPI
    dpi=$(xrdb -query | grep '^Xft.dpi:' | cut -f2)

    if [[ "$dpi" == 96 ]]; then
        export TRAY_MAXSIZE='100%'
        export HEIGHT='2%'
    else
        export TRAY_MAXSIZE=30
        export HEIGHT=30
    fi

    # Start polybar on primary screen with tray
    export MONITOR="$(polybar --list-monitors | grep primary | cut -d: -f1)"
    export TRAY_POSITION=right
    polybar -r top 200>&- &
    disown

    # Start polybar on other screens without tray
    for mon in $(polybar --list-monitors | grep -v primary | cut -d: -f1); do
        export MONITOR="$mon"
        export TRAY_POSITION=none
        polybar -r top 200>&- &
        disown
    done

) 200>/tmp/polybar-launch.lock

exit 0
