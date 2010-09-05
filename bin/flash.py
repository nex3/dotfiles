#!/usr/bin/python

import sys

import pygtk
pygtk.require('2.0')
import gtk
import gobject

DELAY = 10   # Time between fade steps - smaller is faster
STEP  = 0.03 # Jump in opacity
LIMIT = 0.8  # Maximum opacity, between 0 and 1

class FlashWindow:
  def __init__(self):
    self.window = gtk.Window(gtk.WINDOW_POPUP)
    self.window.connect("delete_event", gtk.main_quit)
    self.window.set_default_size(gtk.gdk.screen_width(), gtk.gdk.screen_height())
    self._opacity = 0
    self._fade_in = True

  def set_color(self, color):
    self.window.modify_bg(gtk.STATE_NORMAL, color)

  def show(self):
    self._fade()
    self.window.show()

  def _fade(self):
    self.window.set_opacity(self._opacity)
    if self._fade_in:
      self._opacity += STEP
      if self._opacity >= LIMIT: self._fade_in = False
    else:
      self._opacity -= STEP
      if self._opacity <= 0: gtk.main_quit()
    gobject.timeout_add(DELAY, self._fade)

if __name__ == "__main__":
  if len(sys.argv) == 2:
    window = FlashWindow()
    window.set_color(gtk.gdk.color_parse(sys.argv[1]))
    window.show()
    gtk.main()
  else:
    print """Usage: flash COLOR

Flash the screen COLOR."""
