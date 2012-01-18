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
import XMonad.Layout.Grid
import XMonad.Layout.DwmStyle
import XMonad.Layout.Named
import XMonad.Layout.Reflect
import XMonad.Layout.LayoutHints
import XMonad.Layout.SimplestFloat
--import XMonad.Layout.MagicFocus

import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS -- TODO

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
                          , logHook = dynamicLogWithPP $ myXmobarPP xm
                          , layoutHook = avoidStruts myLayouts
                          , startupHook = setWMName "LG3D" -- a fix for java applets
                          , focusFollowsMouse = False
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
--                       , ("M-<R>", windows W.focusDown)
--                       , ("M-S-<R>", windows W.swapDown)
--                       , ("M-<L>", windows W.focusUp)
--                       , ("M-S-<L>", windows W.swapUp)
                     ]


    myManageHook = composeAll
                   [ className =? "MPlayer"     --> doFloat
                   , className =? "mplayer"     --> doFloat
                   , className =? "Pidgin"      --> moveTo "4:IM"
--                   , className =? "Xmessage" --> (ask >>= doF .  \w -> (\ws -> foldr ($) ws (copyToWss ["4","5"] w) ) . W.shift "6" )
--                    , className =? "Pidgin"      --> moveTo "im"
                   , className =? "Iceweasel" --> moveTo "3:web"
--                    , className =? "Emacs"       --> moveTo "emacs"
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
