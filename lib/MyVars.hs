module MyVars where

import XMonad

data Action = Increase | Decrease

myModMask :: KeyMask
myModMask = mod4Mask

myAppLauncherApp :: String
myAppLauncherApp = "dmenu"

myBrowserApp :: String
myBrowserApp = "google-chrome"

myTerminalApp :: String
myTerminalApp = "gnome-terminal"

myEditorApp :: String
myEditorApp = "emc"

myMailClient :: String
myMailClient = "thunderbird-bin"

volumeAction :: Action -> String
volumeAction Increase = "amixer --card 1 -q set PCM 5%+"
volumeAction Decrease = "amixer --card 1 -q set PCM 5%-"

volumeMasterAction :: Action -> String
volumeMasterAction Increase = "amixer --card 1 -q set Master 5%+"
volumeMasterAction Decrease = "amixer --card 1 -q set Master 5%-"

brightnessAction :: Action -> String
brightnessAction Increase = "xbacklight -steps 1 -time 1 -inc 8"
brightnessAction Decrease = "xbacklight -steps 1 -time 1 -dec 6"
