#!/bin/bash
i3-msg 'focus parent; layout splith; exec urxvt; exec urxvt'
sleep 0.1
i3-msg 'split v; exec urxvt'
sleep 0.1
i3-msg 'focus left; split v; exec urxvt'
sleep 0.1
i3-msg 'focus up'
