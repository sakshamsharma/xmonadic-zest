module Main where
 
import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Util.NamedScratchpad
import XMonad.ManageHook
import XMonad.Util.EZConfig

import XMonad.Hooks.Place
import XMonad.Hooks.EwmhDesktops        (ewmh)
{-import System.Taffybar.Hooks.PagerHints (pagerHints)-}
import System.Posix.Unistd
import XMonad.Util.Run(spawnPipe, safeSpawn)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Util.NamedWindows
import qualified XMonad.StackSet as W

import Keys
import Configs
import Startup
import Layouts
import MyVars

myConfig = ewmh $ defaultConfig {
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
  , modMask = myModMask
  , terminal = myTerminalApp
  } `additionalKeysP` myAdditionalKeys
    `additionalKeys`  myComplexKeys

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

emptyPP = defaultPP { ppOutput = \x-> return () }

main = xmonad =<< statusBar "taffybar" emptyPP toggleStrutsKey myConfig
