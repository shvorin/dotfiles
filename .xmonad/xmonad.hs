{-# LANGUAGE DeriveDataTypeable #-}

import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Util.EZConfig
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Actions.CopyWindow
import XMonad.Layout.PerWorkspace
import XMonad.Layout.IM
import Data.Ratio ((%))
import Maybe (maybe, mapMaybe, fromMaybe)
import Monad (mplus, liftM, ap)
import qualified List
import XMonad.Layout.Grid
import XMonad.Layout.DwmStyle
import XMonad.Layout.Named
import XMonad.Layout.Reflect
import XMonad.Layout.LayoutHints
import XMonad.Layout.SimplestFloat
import qualified XMonad.Util.ExtensibleState as ES
import XMonad.Actions.UpdatePointer
--import XMonad.Layout.MagicFocus

import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS -- TODO
import XMonad.Actions.CycleWindows

import XMonad.Prompt
import XMonad.Prompt.XMonad

-- a fix for java applets
import XMonad.Hooks.SetWMName

import XMonad.Util.Run


myXmobarPP h = xmobarPP { ppOutput = hPutStrLn h }

main = do
  xm <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
  xmonad $ withUrgencyHook NoUrgencyHook $ conf xm where

    conf xm = defaultConfig {
                           terminal = "urxvt"
                          , keys          = \c -> mykeys c `M.union` keys defaultConfig c
                          , manageHook = myManageHook
                          , workspaces = ["1:dev","2:mail","3:web","4:IM","5","6","7","8","9","0"]
                          , logHook = dynamicLogWithPP (myXmobarPP xm) >> updatePointer (Relative 0.2 0.2) >> updateCurrentWList
                          , layoutHook = avoidStruts myLayouts
                          , startupHook = setWMName "LG3D" -- a fix for java applets
                          , focusFollowsMouse = True
                          }

    mykeys conf =
        mkKeymap conf [("M-x g", spawn "firefox")
                      , ("M-x t", spawn $ XMonad.terminal conf)
                      , ("M-C-t", spawn $ XMonad.terminal conf)
                      , ("M-x e", spawn "emacs")
                      , ("<Pause>", spawn "gnome-screensaver-command --lock")
                      , ("M-<Backspace>", focusUrgent)
                      , ("M-v", windows copyToAll)
                      , ("M-S-v", killAllOtherCopies)
                      , ("M-b", sendMessage ToggleStruts)
                      , ("M-,", moveTo Prev NonEmptyWS)
                      , ("M-.", moveTo Next NonEmptyWS)
                      , ("M-p", xmonadPrompt defaultXPConfig)
                      , ("M-C-a", dumpStack)
                      , ("M-<Tab>", myCycleStacks [xK_Alt_L] xK_Tab xK_grave)
                     ]
    xxx s@(W.Stack t ls rs) = s : f W.focusUp' (length ls) ++ f W.focusDown' (length rs) where
        f p 0 = []
        f p 1 = [p s]
        f p n = s : map p (f p (n-1))

    dumpStack :: X ()
    dumpStack = do
      XConf {theRoot = root, display = d} <- ask
      stack <- gets $ W.stack . W.workspace . W.current . windowset
      m <- gets extensibleState
      spawn ("xmessage \"" ++ show stack ++ "\"")

    myManageHook = composeAll
                   [ className =? "MPlayer"     --> doFloat
                   , className =? "mplayer"     --> doFloat
                   , className =? "Pidgin"      --> moveTo "4:IM"
                   , className =? "Iceweasel" --> moveTo "3:web"
                   ]
        where
          moveTo = doF . W.shift
          copyToWss ids win = map (copyWindow win) ids
              

    myLayouts = onWorkspace "4:IM" imLayout $ onWorkspace "3:web" simplestFloat $ onWorkspace "2:mail" simplestFloat $ layoutHints layouts
        where
          layouts = tiled ||| Mirror tiled ||| Full
          tiled   = Tall nmaster delta ratio
          nmaster = 1
          ratio   = 1/2
          delta   = 3/100

          imLayout = named "IM" $ avoidStruts $ reflectHoriz $ IM (1%6) (Role "buddy_list")

data WList = WList (M.Map WorkspaceId [Window]) deriving (Read, Show, Typeable)
instance ExtensionClass WList where
    initialValue = WList M.empty
    extensionType = PersistentExtension

getCurrentWList :: X [Window]
getCurrentWList = do
  WList m <- ES.get
  tag <- gets $ W.tag . W.workspace . W.current . windowset
  return $ fromMaybe [] $ M.lookup tag m

updateCurrentWList :: X ()
updateCurrentWList = do
  WList m <- ES.get
  workspace <- gets $ W.workspace . W.current . windowset
  let tag = W.tag workspace
      stack = W.stack workspace

      fff (W.Stack t ls rs) old =
          t : (curr List.\\ (t:old)) ++ (old `List.intersect` curr)
              where curr = rs ++ ls

  ES.put $ WList (M.alter (Just . (maybe (const []) fff stack) . fromMaybe []) tag m)

myCycleStacks :: [KeySym] -> KeySym -> KeySym -> X ()
myCycleStacks mods keyNext keyPrev = do
    XConf {theRoot = root, display = d} <- ask
    stack <- gets $ W.stack . W.workspace . W.current . windowset
    updateCurrentWList
    wlist <- getCurrentWList


    let stacks = maybe [] (prio wlist) stack

        evt = allocaXEvent $
                  \p -> do maskEvent d (keyPressMask .|. keyReleaseMask) p
                           KeyEvent {ev_event_type = t, ev_keycode = c} <- getEvent p
                           s <- keycodeToKeysym d c 0
                           return (t, s)
        choose n (t, s)
              | t == keyPress   && s == keyNext  = io evt >>= choose (n+1)
              | t == keyPress   && s == keyPrev  = io evt >>= choose (n-1)
              | t == keyPress   && s `elem` [xK_0..xK_9] = io evt >>= choose (numKeyToN s)
              | t == keyRelease && s `elem` mods = return ()
              | otherwise                        = doStack n >> io evt >>= choose n
        doStack n = windows . W.modify' . const $ stacks `cycref` n

    io $ grabKeyboard d root False grabModeAsync grabModeAsync currentTime
    io evt >>= choose 1
    io $ ungrabKeyboard d currentTime
  where cycref l i = l !! (i `mod` length l) -- modify' ensures l is never [], but must also be finite
        numKeyToN = subtract 48 . read . show

        prio wlist (W.Stack t ls rs) = map f wlist
            where
              f w = W.Stack w ls' rs' where
                  Just (ls', rs') = m0 `mplus` m1 `mplus` m2
                  m0 = if w == t then Just (ls, rs) else Nothing
                  m1 = liftM (\(ls1, ls2) -> (ls2, reverse ls1 ++ [t] ++ rs)) $ split ls
                  m2 = liftM (\(rs1, rs2) -> (reverse rs1 ++ [t] ++ ls, rs2)) $ split rs
                  split xs = liftM (\n -> (take n xs, drop (n+1) xs)) $ List.findIndex (==w) xs

