module Main where
 
import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Util.NamedScratchpad
import XMonad.ManageHook

import XMonad.Hooks.Place
import XMonad.Hooks.EwmhDesktops        (ewmh)
import System.Taffybar.Hooks.PagerHints (pagerHints)
import System.Posix.Unistd
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.DynamicLog

import Keys
import Configs
import Startup
import Layouts
import Lemonbar

myConfig = ewmh $ pagerHints $ defaultConfig {
    manageHook = composeAll [
        placeHook myPlacement
        , manageDocks
        , manageHook defaultConfig
        , myManagementHooks
        , manageScratchPad
        , composeAll myFullscreenHooks ]
  , layoutHook = avoidStruts $ smartBorders myLayout
  , keys               = myKeys
  , workspaces         = myWorkspaces
  , startupHook        = myStartup
  , normalBorderColor  = myNormalBorderColor
  , focusedBorderColor = myFocusedBorderColor
  , modMask = mod4Mask
  , terminal = "urxvtc"
  }

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

main = do
  hostname <- fmap nodeName getSystemID
  xmonad =<< statusBar myXmonadlemonbar myLemonHook toggleStrutsKey myConfig
