#!/bin/bash
while [[ "$select" != "NO" && "$select" != "YES" ]]; do
    select=$(echo -e 'NO\nYES' | rofi -color-normal \
        '#101010,#f00060,#131313,#d10047,#101010' \
        -dmenu -i -p "You pressed the exit shortcut. Do you really want to exit i3? This will end your X session.")
    [[ -z "$select" ]] && exit 0
done
[[ "$select" = "NO" ]] && exit 0
i3-msg exit
