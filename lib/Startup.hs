module Startup where

import           XMonad
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Util.Cursor
import           XMonad.Util.SpawnOnce
import qualified XMonad.StackSet as W

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
  spawnOnce "~/.myscripts/blueoff &"
  spawnOnce "synapse -s"
  spawnOnce "emacs --daemon&"
  spawn "feh --bg-fill ~/Wallpapers/hack.jpg &"
  spawn "xrdb -merge ~/.Xresources &"
  spawn "pcmanfm --desktop &"
  spawnToWorkspace "google-chrome-unstable&" "1: Browser"
  spawnToWorkspace "~/n1&" "Mail"

spawnToWorkspace :: String -> String -> X ()
spawnToWorkspace program workspace = do
                                      spawnOnce program     
                                      windows $ W.greedyView workspace
