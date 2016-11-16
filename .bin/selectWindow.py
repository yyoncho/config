#!/usr/bin/env python
import sys
import time
import glib
import gtk
import wnck


windowName = sys.argv[1]

while gtk.events_pending():
    gtk.main_iteration_do(False)

s = wnck.screen_get_default()

s.force_update()


def get_windows():
    for window in s.get_windows():
        if windowName in window.get_name():
            if window != s.get_active_window():
                window.get_workspace().activate(int(time.time() + 1))
                window.activate(int(time.time() + 1))
            else:
                window.minimize()

    main.quit()

main = glib.MainLoop()
glib.idle_add(get_windows)
main.run()
