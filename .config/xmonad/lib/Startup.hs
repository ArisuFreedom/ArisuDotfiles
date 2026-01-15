-- Startup.hs
module Startup
  ( myStartupHook
  ) where

import XMonad
import XMonad.Util.SpawnOnce
import Vars

-- Java/WM
import XMonad.Hooks.SetWMName

-- | Hook
myStartupHook :: X ()
myStartupHook = do

    -- Kill old instances
    spawn "killall trayer"

    -- emacs
    spawn "emacs --daemon"

    -- Compositor
    spawnOnce "picom -b --config ~/.config/picom/picom.conf"

    -- Notifications
    spawnOnce "dunst"

    -- Hide cursor
    spawnOnce "xhidecursor"

    -- Trayer (system tray)
    -- spawnOnce "stalonetray --config ~/.config/stalonetray/stalonetrayrc"
    spawn ("sleep 2 &&" ++ trayer)

    -- Volume icon for trayer
    spawnOnce "volumeicon"

    -- Java/Swing
    setWMName "LG3D"
