module Startup where

import           XMonad
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Util.Cursor
import           XMonad.Util.SpawnOnce
import qualified XMonad.StackSet as W

import Configs

myStartup :: X ()
myStartup = do
  ewmhDesktopsStartup
  setDefaultCursor xC_left_ptr
  spawnOnce "taffybar&"
  spawnToWorkspace (myBrowser ++ "&") "1: Browser"
  spawnOnce "nm-applet &"
  spawnOnce "pasystray &"
  spawnOnce "xscreensaver -no-splash &"
  spawnOnce "watch -n 120 ~/.myscripts/batteryNotifier.sh &"
  spawnOnce "~/.myscripts/blueoff &"
  spawn "feh --bg-fill ~/Wallpapers/wallpaper.jpg &"
  spawn "xrdb -merge ~/.Xresources &"
  spawn "pcmanfm --desktop &"
  spawnToWorkspace (myMailClient ++ "&") "Mail"

spawnToWorkspace :: String -> String -> X ()
spawnToWorkspace program workspace = do
                                      spawnOnce program     
                                      windows $ W.greedyView workspace
