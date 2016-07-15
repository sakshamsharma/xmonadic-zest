module Lemonbar where

import System.IO (hPutStrLn)
import XMonad.Hooks.DynamicLog hiding (xmobar, xmobarPP, xmobarColor, sjanssenPP, byorgeyPP)
import XMonad.Util.Loggers

myBatteryFormat = onLogger $ wrap "%{B#004411}%{F#eeeeee} " " %{B#000000}%{F#2199ee}"
myLoadFormat = onLogger $ wrap "%{B#001144} CPU: %{F#eeeeee} " "% %{B#000000}%{F#2199ee}" . takeWhile (/= ' ') . drop 2

myLemonHook = defaultPP {
    ppCurrent           =  wrap "%{R}" "%{R}"  . pad
  , ppVisible          = wrap " " " " . pad
  , ppHidden           = wrap "%{+u}" "%{-u}"  . pad 
  , ppWsSep            = "%{-u}  "
  , ppSep              = " "
  , ppTitle = wrap "%{F#00ff00}" "%{r}" .
      (\x -> case x of
          ""   -> "..."
          _    -> x
          )
  , ppLayout              = wrap " " " " .
      (\x -> case x of
      "Maximize Minimize mfullscreen"  -> "   %{B#00ff00}%{F#000000}  full  %{B#000000}%{F#2199ee}   "
      "Maximize Minimize mgrid"        -> "   %{B#00ff00}%{F#000000}  grid  %{B#000000}%{F#2199ee}   "
      "Maximize Minimize mtabbed"      -> "   %{B#00ff00}%{F#000000}  tabb  %{B#000000}%{F#2199ee}   "
      "Maximize Minimize mtiled"       -> "   %{B#00ff00}%{F#000000}  tile  %{B#000000}%{F#2199ee}   "
      _                                -> "%{B#007733}%{F#999999} " ++ x ++ " %{B#000000}%{F#2199ee}"
      ) 
  , ppOrder       =  \(ws:l:t:x) -> [l, ws, "  " , t] ++ x
  , ppExtras      =  [myLoadFormat loadAvg, myBatteryFormat battery, date "%a %b %d %H:%M "]
}

myXmonadlemonbar = "lemonbar -d -p -f \"Source Code Pro For Powerline:size=12\" -F \"#2199ee\" -f \"monofur for Powerline:size=11\" -B \"#000000\" -u 4 -U \"#00ff00\" > /dev/null"
