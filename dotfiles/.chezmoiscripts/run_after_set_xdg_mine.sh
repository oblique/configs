#!/bin/bash

if [[ -e /usr/bin/xdg-settings ]]; then
    xdg-settings set default-web-browser brave-browser.desktop
fi
