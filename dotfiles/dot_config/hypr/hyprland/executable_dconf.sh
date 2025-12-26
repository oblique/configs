#!/bin/bash

echo AAA > /tmp/bla

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

[org/gnome/desktop/default-applications/terminal]
exec='/usr/bin/alacritty'

[org/gnome/desktop/wm/preferences]
button-layout=''
"

echo "$conf" | dconf load -f /
