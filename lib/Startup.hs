module Startup where

import           XMonad
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Util.Cursor
import           XMonad.Util.SpawnOnce
import qualified XMonad.StackSet as W

-- | Sleeps for provided time, then
--   Kills running instances of program (if greedyKill)
--   Then starts the program if greedyKill was enabled, or
--   if process wasn't running already
delayedStartOnce :: Bool -> Int -> String -> X ()
delayedStartOnce greedyKill time run =
  let execName = takeWhile (/= ' ') run
      sleep = "sleep " ++ show time
      kills = "pkill " ++ execName
      ifkill = "if not pgrep " ++ execName ++ "; then " ++ run ++ "; fi;"
      ands = "; "
  in if greedyKill then
      spawn $ "(" ++ sleep ++ ands ++ kills ++ ands ++ run ++ ") &"
    else
      spawn $ "(" ++ sleep ++ ands ++ ifkill ++ ") &"

myStartup :: X ()
myStartup = do
  ewmhDesktopsStartup
  setDefaultCursor xC_left_ptr
  delayedStartOnce True 00 "taffybar"
  delayedStartOnce True 10 "nm-applet"
  delayedStartOnce True 10 "pasystray"
  delayedStartOnce True 15 "thunderbird"
  delayedStartOnce False 30 "xscreensaver -no-splash"
  delayedStartOnce False 05 "emacs --daemon"
  delayedStartOnce False 02 "urxvtd --quiet --opendisplay --fork"
  delayedStartOnce False 30 "watch -n 120 ~/.myscripts/batteryNotifier.sh"
  spawnOnce "~/.myscripts/blueoff &"
  spawn "feh --bg-fill ~/Wallpapers/wallpaper.jpg &"
  spawn "xrdb ~/.Xresources &"
  spawn "pcmanfm --desktop &"

spawnToWorkspace :: String -> String -> X ()
spawnToWorkspace program workspace = do
                                      spawnOnce program
                                      windows $ W.greedyView workspace
