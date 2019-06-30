module Main where

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Util.NamedScratchpad
import XMonad.ManageHook
import XMonad.Util.EZConfig

import XMonad.Hooks.Place
import XMonad.Hooks.EwmhDesktops        (ewmh)
-- import System.Taffybar.Hooks.PagerHints (pagerHints)
import System.Posix.Unistd
import XMonad.Util.Run(spawnPipe, safeSpawn)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Util.NamedWindows
import qualified XMonad.StackSet as W
import Graphics.X11.Xinerama (getScreenInfo)

import XMonad.Util.WorkspaceCompare
import XMonad.Actions.PhysicalScreens
import XMonad.Layout.IndependentScreens
import System.IO

import Keys
import Configs
import Startup
import Layouts
import MyVars

myUrgencyConfig = urgencyConfig { suppressWhen = Focused
                                , remindWhen = Every 30
                                }

myConfig = withUrgencyHookC NoUrgencyHook myUrgencyConfig $ docks $ ewmh $ {- pagerHints $ -} defaultConfig {
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

xineramaXmobarPP = xmobarPP { ppSort = getSortByXineramaPhysicalRule horizontalScreenOrderer
                            , ppUrgent = xmobarColor "yellow" "red" . wrap ">" "<" . xmobarStrip
                            }

main = do
  n <- countScreens
  xmprocs <- mapM (\i -> spawnPipe $ "xmobar -x" ++ show i) [0..n-1]
  let myLogHook = mapM_ (\handle -> dynamicLogWithPP $ xineramaXmobarPP { ppOutput = hPutStrLn handle }) xmprocs
  xmonad $ myConfig { logHook = myLogHook }
