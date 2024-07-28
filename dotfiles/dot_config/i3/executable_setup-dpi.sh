#!/bin/bash

gsettings set org.gnome.desktop.interface text-scaling-factor 1.33
sleep 0.2

# Apply DPI that was generated from `gsettings` to `xrandr`
dpi="$(xrdb -query | grep Xft.dpi: | cut -f2)"
[[ -n "$dpi" ]] && xrandr --dpi "$dpi"
