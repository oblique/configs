#!/bin/bash

# to enable it go to skype options -> notifications
# click on advanced view, choose 'First Chat Message Received'
# and put the absolute path of the script in 'Execute the following script'
# do the same for 'Chat Message Received'
# the script will not be executed when your status is Busy

wid=''

for x in $(xdotool search --class skype); do
    wm_name=$(xprop -id $x WM_NAME)
    wm_role=$(xprop -id $x WM_WINDOW_ROLE)
    map_state=$(xwininfo -id $x | grep 'Map State')
    if [[ $wm_name =~ .*\ -\ Skype.* ]]; then
        [[ $map_state =~ .*IsUnMapped.* ]] && continue
        if [[ $wm_role =~ .*ConversationsWindow.* ]]; then
            wid=$x
            break
        fi
        wid=$x
    fi
done

[[ -n $wid ]] && xdotool set_window --urgency 1 $wid
