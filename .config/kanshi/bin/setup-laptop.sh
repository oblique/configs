#!/bin/bash
set -e

source ~/.config/kanshi/bin/common.sh

# adjust font size
gsettings set org.gnome.desktop.interface text-scaling-factor 1.33
set_alacritty_font_size 16
