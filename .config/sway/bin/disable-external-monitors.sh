#!/bin/bash
set -e

swaymsg output eDP-1 enable

swaymsg -t get_outputs -r | jq -r '.[].name' | while read mon; do
    [[ "$mon" == "eDP-1" ]] && continue
    swaymsg output "$mon" disable
done
