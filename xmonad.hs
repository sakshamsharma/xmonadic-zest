import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders(smartBorders)

import XMonad.Hooks.Place
import XMonad.Hooks.EwmhDesktops        (ewmh)
import System.Taffybar.Hooks.PagerHints (pagerHints)

import Keys
import Configs
import Startup

main =
  xmonad $ ewmh $ pagerHints $ defaultConfig {
    manageHook = placeHook myPlacement <+> manageDocks <+> manageHook defaultConfig <+> myManagementHooks <+> composeAll myFullscreenHooks
  , layoutHook = avoidStruts $ smartBorders myLayout
  , keys               = myKeys
  , workspaces         = myWorkspaces
  , startupHook        = myStartup
  , normalBorderColor  = myNormalBorderColor
  , focusedBorderColor = myFocusedBorderColor
  , modMask = mod4Mask
  , terminal = "urxvt"
  }
