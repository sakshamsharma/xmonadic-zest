module MyVars where

import XMonad

data Action = Increase | Decrease

myModMask :: KeyMask
myModMask = mod4Mask

myAppLauncherApp :: String
myAppLauncherApp = "rofi -show run -lines 6 -eh 1 -width 100 -padding 100 -opacity \"85\" -bw 0 -bc \"#2f343f\" -bg \"#2f343f\" -fg \"#f9f9f9\" -hlbg \"#2f343f\" -hlfg \"#9575cd\""

myBrowserApp :: String
myBrowserApp = "google-chrome-beta"

myTerminalApp :: String
myTerminalApp = "urxvtc"

myEditorApp :: String
myEditorApp = "emc"

myMailClient :: String
myMailClient = "thunderbird-bin"

volumeAction :: Action -> String
volumeAction Increase = "amixer --card 1 -q set PCM 5%+"
volumeAction Decrease = "amixer --card 1 -q set PCM 5%-"

brightnessAction :: Action -> String
brightnessAction Increase = "xbacklight -steps 1 -time 1 -inc 8"
brightnessAction Decrease = "xbacklight -steps 1 -time 1 -dec 6"
