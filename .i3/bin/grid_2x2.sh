#!/bin/bash
i3-msg 'focus parent; layout splith'
~/.i3/bin/i3-exec-wait.pl urxvt
~/.i3/bin/i3-exec-wait.pl urxvt
i3-msg 'split v'
~/.i3/bin/i3-exec-wait.pl urxvt
i3-msg 'focus left; split v'
~/.i3/bin/i3-exec-wait.pl urxvt
i3-msg 'focus up'
