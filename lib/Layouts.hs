{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction #-}

module Layouts where
 
import           XMonad
import qualified XMonad.StackSet as S
import           XMonad.Layout.Spacing
import qualified XMonad.Layout.Tabbed as Tab
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.Grid
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.Reflect
import           XMonad.Util.WindowProperties
import           Control.Monad
import           Data.Ratio

myLayout = stiled ||| tiled ||| Mirror tiled ||| Tab.simpleTabbed
 where
  -- default tiling algorithm partitions the screen into two panes
  tiled = Tall nmaster1 delta ratio

  stiled = spacing 5 $ Tall nmaster1 delta ratio

  nmaster1 = 1  -- The default number of windows in the master pane
  ratio = 1/2   -- Default proportion of screen occupied by master pane
  delta = 5/100 -- Percent of screen to increment by when resizing panes


-- | The following has been taken (and slightly modified) from an online source.
--   It is certainly not the nice solution I was looking for.
--   And I believe it doesn't even work.
--   TO BE REPLACED

imLayout = avoidStruts $ reflectHoriz $ withIMs ratio rosters chatLayout where
    chatLayout      = Grid
    ratio           = 1%6
    rosters         = [skypeRoster, pidginRoster]
    pidginRoster    = And (ClassName "Pidgin") (Role "buddy_list")
    skypeRoster     = (ClassName "Skype") `And` (Not (Title "Options")) `And` (Not (Role "Chats")) `And` (Not (Role "CallWindowForm"))
 
-- modified version of XMonad.Layout.IM --
 
-- | Data type for LayoutModifier which converts given layout to IM-layout
-- (with dedicated space for the roster and original layout for chat windows)
data AddRosters a = AddRosters Rational [Property] deriving (Read, Show)
 
instance LayoutModifier AddRosters Window where
  modifyLayout (AddRosters ratio props) = applyIMs ratio props
  modifierDescription _                = "IMs"
 
-- | Modifier which converts given layout to IMs-layout (with dedicated
-- space for rosters and original layout for chat windows)
withIMs :: LayoutClass l a => Rational -> [Property] -> l a -> ModifiedLayout AddRosters l a
withIMs ratio props = ModifiedLayout $ AddRosters ratio props
 
-- | IM layout modifier applied to the Grid layout
gridIMs :: Rational -> [Property] -> ModifiedLayout AddRosters Grid a
gridIMs ratio props = withIMs ratio props Grid
 
hasAnyProperty :: [Property] -> Window -> X Bool
hasAnyProperty [] _ = return False
hasAnyProperty (p:ps) w = do
    b <- hasProperty p w
    if b then return True else hasAnyProperty ps w
 
-- | Internal function for placing the rosters specified by
-- the properties and running original layout for all chat windows
applyIMs :: (LayoutClass l Window) =>
               Rational
            -> [Property]
            -> S.Workspace WorkspaceId (l Window) Window
            -> Rectangle
            -> X ([(Window, Rectangle)], Maybe (l Window))
applyIMs ratio props wksp rect = do
    let stack = S.stack wksp
    let ws = S.integrate' $ stack
    rosters <- filterM (hasAnyProperty props) ws
    let n = fromIntegral $ length rosters
    let (rostersRect, chatsRect) = splitHorizontallyBy (n * ratio) rect
    let rosterRects = splitHorizontally n rostersRect
    let filteredStack = stack >>= S.filter (`notElem` rosters)
    (a,b) <- runLayout (wksp {S.stack = filteredStack}) chatsRect
    return (zip rosters rosterRects ++ a, b)
