#!/usr/bin/env python3
import i3ipc


def on_window(i3, e):
    if ((e.container.window_class != 'chromium' and
         e.container.window_class != 'chrome') or
        e.container.window_role != 'pop-up' or
        e.container.name != 'Tabs Outliner'):
        return

    # remove border
    # move Tabs Outliner to the left of Chromium
    # shrink it
    cmds = ''
    cmds += 'border pixel 0;'
    cmds += 'move left;'
    for x in range(0, 20):
        cmds += 'resize shrink right;'
    cmds += 'resize grow right;'

    e.container.command(cmds)


i3 = i3ipc.Connection()
i3.on('window::new', on_window)
i3.on('window::title', on_window)
i3.main()
