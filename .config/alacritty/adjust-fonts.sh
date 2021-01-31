#!/bin/bash

if [[ $# -ne 1 ]]; then
    echo "$0 [fhd|qhd]"
    exit 1
fi

if [[ "$1" == "fhd" ]]; then
    export FONT_SIZE=12
elif [[ "$1" == "qhd" ]]; then
    export FONT_SIZE=16
else
    echo "$0 [fhd|qhd]"
    exit 1
fi

# Create font config
cat > ~/.config/alacritty/fonts.yml <<EOF
font:
    normal:
        family: Hermit
    size: $FONT_SIZE
EOF

# Trigger config reload
touch ~/.config/alacritty/alacritty.yml
