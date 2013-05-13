#!/bin/sh

ps -U $(whoami) -o command | grep "^$*\$" > /dev/null 2>&1 || {
    "$@" &
}
