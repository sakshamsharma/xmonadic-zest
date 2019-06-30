module Configs where

import           XMonad
import           XMonad.Layout.PerWorkspace
import           XMonad.Hooks.Place
import           XMonad.Hooks.ManageHelpers
import           XMonad.Prompt
import           XMonad.Prompt.Input
import qualified XMonad.StackSet as W
import           XMonad.Util.Scratchpad

import           Data.Maybe
import           System.Exit
import qualified Data.Map as M
import qualified Data.List as L

import           Layouts
import           MyVars()

-- special command prompt
commandPrompt :: XPConfig -> String -> M.Map String (X ()) -> X ()
commandPrompt c p m = inputPromptWithCompl c p (mkComplFunFromList (M.keys m)) ?+ (\k -> fromMaybe (return ()) (M.lookup k m))

commands :: M.Map String (X ())
commands = M.fromList
  [ ("logout"       , io    exitSuccess)
  , ("lock"         , spawn "xscreensaver-command -lock")
  , ("suspend"      , spawn "xscreensaver-command -lock && sleep 2 && sudo systemctl suspend -i")
  , ("shutdown"     , spawn "sleep 2 && systemctl poweroff")
  , ("restart"      , spawn "sleep 2 && systemctl reboot")
  , ("sleep"        , spawn "xscreensaver-command -lock && sleep 1 && sudo pm-suspend")
  ]

-- shellprompt config
fireSPConfig :: XPConfig
fireSPConfig = def
  { bgColor             = colorFocusBG,
    fgColor             = colorNormalFG,
    bgHLight            = colorNormalBG,
    fgHLight            = colorFocusFG,
    borderColor         = "black",
    promptBorderWidth   = 0,
    position            = Bottom,
    height              = 12,
    historySize         = 256,
    defaultText         = "",
    autoComplete        = Nothing
  }

myWorkspaces :: [String]
myWorkspaces    = ["1","2","3","4","5","6","7","8","9","0"]

myFullscreenHooks = [ composeOne [ isFullscreen -?> doFullFloat  ] ]

myPlacement = withGaps (0,0,0,0) (smart (0.5,0.5))

myManagementHooks = composeAll . concat $
    [ [ title       =? t                  -->
        doFloat              | t <- myOtherFloats]
    , [ fmap ( c `L.isInfixOf`) className -->
               doShift (head myWorkspaces) | c <- myBrowsers]
    , [ className   =? c                  -->
               doShift (myWorkspaces !! 1) | c <- ["Emacs"]]
    , [ className   =? c                  -->
               doShift (myWorkspaces !! 3) | c <- myMusic]
    , [ fmap ( c `L.isInfixOf`) className -->
               doShift "IM"                | c <- imApps]
    , [ className   =? c                  -->
               doShift "Mail"              | c <- ["Thunderbird"] ]
    ]
  where myOtherFloats = ["alsamixer", "chrome-app-list", "cappl", "htop", "nmtui"]
        imApps        = ["Skype", "Pidgin"]
        myBrowsers    = ["Firefox", "Chrome", "google-chrome-beta"]
        myMusic       = ["Rhythmbox", "Banshee", "Spotify"]

-- These layouts are stored in the Custom.Layouts module
myLayoutHook = myLayout

manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
  where
    h = 0.1     -- terminal height, 10%
    w = 1       -- terminal width, 100%
    t = 1 - h   -- distance from top edge, 90%
    l = 1 - w   -- distance from left edge, 0%

-- color definitions
myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#000000"

-- colors for shellprompt
colorNormalFG = "#B6B4B8"
colorNormalBG = "#2F2E2B"
colorNormalBO = "#1C2636"
colorFocusFG = "#FFFFFF"
colorFocusBG = "#2F2E2B"
colorFocusBO = "#FF0000"
colorOtherFG = "#707070"
colorOtherBG = "#2F2E2B"
