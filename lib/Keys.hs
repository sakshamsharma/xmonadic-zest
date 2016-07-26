module Keys where

import System.Exit
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Prompt.Shell
import XMonad.Hooks.Place
import Graphics.X11.ExtraTypes.XF86
import XMonad.Prompt.Window
import XMonad.Actions.WindowBringer
import XMonad.Prompt
import XMonad.Prompt.AppLauncher as AL
import XMonad.Util.Scratchpad

import qualified Data.Map        as M
import qualified XMonad.StackSet as W
import XMonad.Actions.SwapWorkspaces (swapWithCurrent)

import Configs
import MyVars

myAdditionalKeys :: [([Char], X())]
myAdditionalKeys =
  [ ("M-S-<Up>", windows W.swapUp)
  , ("M-S-<Down>", windows W.swapDown)
  , ("M1-<Space>", gotoMenu)
  , ("M1-S-<Space>", bringMenu)
  , ("M1-<Up>", prevWS)
  , ("M1-<Down>", nextWS)
  , ("M-S-q", kill)
  , ("M-<Space>", sendMessage NextLayout)
  , ("M-<Down>", windows W.focusDown)
  , ("M-<Up>", windows W.focusUp)
  , ("M-j", windows W.focusDown)
  , ("M-k", windows W.focusUp)
  , ("M-S-<Return>", windows W.swapMaster)
  , ("M-z", toggleWS)
  , ("M-t", withFocused $ windows . W.sink)

  -- Launchers
  , ("M-d", spawn myAppLauncherApp)
  , ("M-<Return>", spawn myTerminalApp)
  , ("M-s", scratchpadSpawnActionTerminal myTerminalApp)
  , ("M-x f", spawn myBrowserApp)
  , ("M1-<Return>", spawn myEditorApp)
  , ("M-p", AL.launchApp def "evince")
  , ("M-S-l", spawn "xscreensaver-command -lock")

  -- Media keys
  , ("<XF86AudioLowerVolume>", spawn $ volumeAction Decrease)
  , ("<XF86AudioRaiseVolume>", spawn $ volumeAction Increase)
  , ("M1-3", spawn $ volumeAction Decrease)
  , ("M1-4", spawn $ volumeAction Increase)
  , ("<XF86MonBrightnessDown>", spawn $ brightnessAction Decrease)
  , ("<XF86MonBrightnessUp>", spawn $ brightnessAction Increase)
  ]

-- | Keys which don't exist in the simple default string mappings above
myComplexKeys :: [((KeyMask, KeySym), X())]
myComplexKeys =
  [ ((mod4Mask, xK_F12   ), commandPrompt fireSPConfig "command" commands)
  , ((mod4Mask, xK_h     ), sendMessage Shrink) -- Shrink master area
  , ((mod4Mask, xK_l     ), sendMessage Expand) -- Expand master area
  , ((mod4Mask, xK_comma ), sendMessage (IncMasterN 1)) -- Increment master win cnt
  , ((mod4Mask, xK_period), sendMessage (IncMasterN (-1))) -- Decrement master count
  , ((mod4Mask, xK_F1), spawn myBrowserApp)
  , ((mod4Mask, xK_F2), spawn "urxvt -e nmtui")
  , ((mod4Mask, xK_F3), spawn "pcmanfm")
  , ((mod4Mask, xK_F4), spawn "urxvt -e alsamixer")
  ]


-- | Key bindings. Add, modify or remove key bindings here.
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@ XConfig {XMonad.modMask = modm} = M.fromList $
  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [
    ((m .|. modm, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]
  ++
  -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
  [
    ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  ]
  ++
  -- if you're on workspace 1, hitting mod-ctrl-5 will swap workspaces 1 and 5
  [
    ((modm .|. controlMask, k), windows $ swapWithCurrent i)
      | (i, k) <- zip myWorkspaces [xK_1 ..]
  ]


-- | Mouse bindings: default actions bound to mouse events
mouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
mouseBindings XConfig {XMonad.modMask = modm} = M.fromList
    -- mod-button1 %! Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                          >> windows W.shiftMaster)
    -- mod-button2 %! Raise the window to the top of the stack
    , ((modm, button2), windows . (W.shiftMaster .) . W.focusWindow)
    ]
