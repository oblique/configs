#!/bin/sh

# Simple script to use youtube-dl for all youtube links
# and your browser of choice otherwise

BROWSER=firefox
export LINK=$@

if [ -z ${LINK} ]; then
    echo "usage: $0 <link>"
    exit 1
fi

echo ${LINK} | grep -ie "^https\?://\(\(\(www.\)\?youtube.com/watch\(?\|.*&\)v=.\+\)\|\(youtu.be/.\+\)\)"
if [ $? -eq 0 ]; then
    urxvt -e sh -c 'mplayer $(youtube-dl -g ${LINK})' &
else
    ${BROWSER} ${LINK} &
fi
