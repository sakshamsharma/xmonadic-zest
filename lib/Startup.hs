module Startup where

import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Cursor
import XMonad.Util.SpawnOnce

myStartup :: X ()
myStartup = do
  ewmhDesktopsStartup
  setDefaultCursor xC_left_ptr
  spawnOnce "~/.cache/taffybar/taffybar-linux-x86_64 &"
  spawnOnce "nm-applet &"
  spawnOnce "pasystray &"
  spawnOnce "xscreensaver -no-splash &"
  spawnOnce "conky -c ~/.conky/sideconky &"
  spawnOnce "watch -n 120 ~/.myscripts/batteryNotifier.sh &"
  spawnOnce "emacs --daemon &"
  spawnOnce "~/.myscripts/blueoff &"
  spawnOnce "synapse -s"
  spawn "feh --bg-fill ~/Wallpapers/4.jpg &"
  spawn "feh --bf-fill ~/Wallpapers/asscreed.jpg &"
  spawn "xrdb -merge ~/.Xresources &"
  spawn "pcmanfm --desktop &"
