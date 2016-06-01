module Main where
 
import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Util.NamedScratchpad
import XMonad.ManageHook

import XMonad.Hooks.Place
import XMonad.Hooks.EwmhDesktops        (ewmh)
import XMonad.Util.Run (runProcessWithInput)

import System.Environment (setEnv, unsetEnv)
import System.Taffybar.Hooks.PagerHints (pagerHints)
import System.Posix.Unistd

import Keys
import Configs
import Startup
import Layouts

main = do
  _ <- setGhcPkgPath "/home/saksham/.stack/global-project/stack.yaml" "/home/saksham/.stack/global-project/.stack-work/install/x86_64-linux/lts-5.17/7.10.3/pkgdb:/home/saksham/.stack/snapshots/x86_64-linux/lts-5.17/7.10.3/pkgdb:/home/saksham/.stack/programs/x86_64-linux/ghc-7.10.3/lib/ghc-7.10.3/package.conf.d"
  hostname <- fmap nodeName getSystemID
  xmonad $ ewmh $ pagerHints $ defaultConfig {
    manageHook = placeHook myPlacement <+> manageDocks <+> manageHook defaultConfig <+> myManagementHooks <+> composeAll myFullscreenHooks <+> manageScratchPad
  , layoutHook = avoidStruts $ smartBorders myLayout
  , keys               = myKeys
  , workspaces         = myWorkspaces
  , startupHook        = myStartup
  , normalBorderColor  = myNormalBorderColor
  , focusedBorderColor = myFocusedBorderColor
  , modMask = mod4Mask
  , terminal = "urxvtc"
  , browser = "google-chrome-beta"
  }

setGhcPkgPath :: String -> String -> IO ()
setGhcPkgPath config global = do
  let stackPath p = runProcessWithInput
        "stack"
        ["--stack-yaml", config, "path", p]
        ""
  localOut <- stackPath "--local-pkg-db"
  snapshotOut <- stackPath "--snapshot-pkg-db"
  case (lines localOut, lines snapshotOut) of
    ((local:_), (snapshot:_)) -> do
       let pkgPath = local ++ ":" ++ snapshot ++ ":" ++ global
       putStrLn $ "Temporarily setting GHC_PACKAGE_PATH=" ++ pkgPath
       setEnv "GHC_PACKAGE_PATH" pkgPath
    -- FIXME: after recompile, this seems to happen, for some reason.
    _ -> liftIO $ putStrLn "Failed to get package dbs from stack"

-- Put this in your startup hook
unsetGhcPkgPath :: X ()
unsetGhcPkgPath = liftIO $ unsetEnv "GHC_PACKAGE_PATH"
