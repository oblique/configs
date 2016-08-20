#!/usr/bin/env python3
import i3ipc
import time


def on_window(i3, e):
    if e.container.window_instance != 'crx_eggkanocgddhmamlbiijnphhppkpkmkl':
        return

    cmds = ''
    # remove border
    cmds += 'border pixel 0;'
    # move Tabs Outliner to the right of Chromium
    cmds += 'move right;'
    # shrink it to the minimum
    # NOTE: we can not use '1000 ppt' because i3 will ignore it.
    #       to be able to reach the smallest width we need to shrink
    #       with '1 ppt' per time
    cmds += 'resize shrink width 0 px or 1 ppt;' * 1000
    # grow it the size we want
    cmds += 'resize grow width 0 px or 9 ppt;'

    e.container.command(cmds)


while True:
    i3 = i3ipc.Connection()
    i3.on('window::new', on_window)
    i3.on('window::title', on_window)
    i3.main()
    i3.main_quit()
    time.sleep(1)
