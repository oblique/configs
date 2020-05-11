#!/bin/bash
set -e

source ~/.config/kanshi/bin/common.sh

# adjust font size
gsettings set org.gnome.desktop.interface text-scaling-factor 1.33
set_alacritty_font_size 16

# move workspace 1-9 to primary monitor
for x in $(seq 1 9); do
    swaymsg workspace $x
    swaymsg move workspace to '"Unknown HP Z27 CN49500CXB"'
done

# move workspace 10 to secondary monitor
swaymsg workspace 10
swaymsg move workspace to eDP-1

# focus to primary monitor
swaymsg workspace next
swaymsg workspace 1
