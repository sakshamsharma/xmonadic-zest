module Main where
 
import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Util.Run(spawnPipe)

import XMonad.Hooks.Place
import XMonad.Hooks.EwmhDesktops        (ewmh)
import System.Taffybar.Hooks.PagerHints (pagerHints)

import Keys
import Configs
import Startup
import Layouts
import Lemonbar

main :: IO()
main = do
  lemonbar <- spawnPipe myXmonadlemonbar
  _ <- spawnPipe myXmonadTrayer
  xmonad $ ewmh $ pagerHints $ def {
    manageHook = placeHook myPlacement <+> manageDocks <+> manageHook def <+> myManagementHooks <+> composeAll myFullscreenHooks <+> manageScratchPad
  , layoutHook = avoidStruts $ smartBorders myLayout
  , keys               = myKeys
  , workspaces         = myWorkspaces
  , startupHook        = myStartup
  , normalBorderColor  = myNormalBorderColor
  , focusedBorderColor = myFocusedBorderColor
  , modMask = mod4Mask
  , terminal = "urxvtc"
  , logHook = myLogHook lemonbar
  }
