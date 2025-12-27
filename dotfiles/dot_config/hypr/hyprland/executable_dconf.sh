#!/bin/bash

conf="
[org/gnome/desktop/interface]
icon-theme='Tela-circle-dracula'
gtk-theme='Catppuccin-Mocha'
color-scheme='prefer-dark'
cursor-theme='Adwaita'
cursor-size=24
font-name='Cantarell 10'
document-font-name='Cantarell 10'
monospace-font-name='CaskaydiaCove Nerd Font Mono 9'
font-antialiasing='rgba'
font-hinting='full'
text-scaling-factor=1.0

[org/gnome/desktop/privacy]
remember-recent-files=false

[org/gnome/desktop/thumbnailers]
disable-all=true

[org/gnome/desktop/sound]
event-sounds=true
input-feedback-sounds=false

[org/gnome/desktop/default-applications/terminal]
exec='/usr/bin/alacritty'

[org/gnome/desktop/wm/preferences]
button-layout=''
"

echo "$conf" | dconf load -f /
