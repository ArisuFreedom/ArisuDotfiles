{-# OPTIONS_GHC -Wno-deprecations #-}

-- xmonad.hs
import System.Exit (exitWith, ExitCode(ExitSuccess))
import XMonad
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isDialog, doCenterFloat)
import XMonad.Hooks.Modal
import XMonad.Hooks.RefocusLast (refocusLastLogHook)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Fullscreen
import XMonad.Layout.IndependentScreens
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Spacing
import XMonad.Layout.ToggleLayouts
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.SpawnOnce
import qualified XMonad.Util.Hacks as Hacks

-- My libs
import Vars
import Theme
import Scratchpads
import Keys
import Startup
import Manage
import Layouts
import Topic
import StatusBar

main :: IO ()
main = xmonad
      . Hacks.javaHack
      . ewmhFullscreen
      . ewmh
      . withSB mySB
      . docks
      $ myConfig

myConfig = def
    { modMask            = modMask'
    , terminal           = terminal'
    , borderWidth        = myBorderWidth
    , focusedBorderColor = myFocusedBorderColor
    , normalBorderColor  = myNormalBorderColor
    , layoutHook         = myLayoutHook
    , manageHook         = manageDocks
                        <+> myManageHook
                        <+> namedScratchpadManageHook myScratchPads

    , handleEventHook    = fullscreenEventHook
                         <> Hacks.windowedFullscreenFixEventHook
                         <> Hacks.trayerAboveXmobarEventHook
                         <> Hacks.trayerPaddingXmobarEventHook
                         -- <> Hacks.fixSteamFlicker

    , workspaces         = topicNames topicItems
    , logHook            = dynamicLog
                           >> nsSingleScratchpadPerWorkspace myScratchPads
                           >> updatePointer (0.5, 0.5) (0, 0) -- (0.5, 0.5)
    , startupHook        = myStartupHook
    } `additionalKeysP` myKeyBindings
