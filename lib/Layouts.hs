{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction #-}

module Layouts where

import           XMonad
import qualified XMonad.StackSet as S
import           XMonad.Layout.Spacing
import           XMonad.Layout.Simplest
import           XMonad.Layout.Tabbed
import           XMonad.Layout.Decoration
import           XMonad.Layout.SimpleDecoration (shrinkText)
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.Grid
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Reflect
import           XMonad.Util.WindowProperties
import           Control.Monad
import           Data.Ratio

myTabbedLayout :: ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest Window
myTabbedLayout = tabbed shrinkText def { fontName = "xft:Source Code Pro For Powerline:size=10"
                                       , activeTextColor = "#ffffff"
                                       , activeColor = "#0087ff"
                                       , urgentTextColor = "#ff3700"
                                       }

myLayout = stiled ||| myTabbedLayout ||| Mirror stiled
 where
  -- default tiling algorithm partitions the screen into two panes
  {-tiled = ResizableTall nmaster1 delta ratio _slaves-}

  stiled = spacing 5 $ ResizableTall nmaster1 delta ratio _slaves

  nmaster1 = 1  -- The default number of windows in the master pane
  ratio = 1/2   -- Default proportion of screen occupied by master pane
  delta = 5/100 -- Percent of screen to increment by when resizing panes
  _slaves = []
