-- Startup.hs
module Startup
  ( myStartupHook
  ) where

import XMonad
import XMonad.Util.SpawnOnce
import Vars

-- Para compatibilidade Java/WM
import XMonad.Hooks.SetWMName

-- |
myStartupHook :: X ()
myStartupHook = do

    -- Kill old instances
    spawn "killall trayer"

    -- emacs
    spawn "emacs --daemon"

    -- Display & monitor setup
    spawnOnce "xrandr --output DisplayPort-2 --mode 1920x1080 --rate 100 --rotate normal --brightness 1.0"

    -- Wallpaper
    spawnOnce "xwallpaper --zoom ~/Pictures/Wallpapers/lonely-house.jpg"

    -- Compositor
    spawnOnce "picom -b --config ~/.config/picom/picom.conf"

    -- Repeat rate do teclado
    spawnOnce "xset r rate 300 50"

    -- Disable Caps Lock
    spawnOnce "setxkbmap -option caps:none"

    -- Remove anoying beep sounds
    spawnOnce "xset b off"

    -- Notifications
    spawnOnce "dunst"

    -- Normal cursor instead of X one
    spawnOnce "xsetroot -cursor_name left_ptr"

    -- Hide cursor
    spawnOnce "xhidecursor"

    -- Auto-locker
    spawnOnce "xautolock -time 10 -locker slock"

    -- Trayer (system tray)
    -- spawnOnce "stalonetray --config ~/.config/stalonetray/stalonetrayrc"
    spawn ("sleep 2 &&" ++ trayer)

    -- Volume icon for trayer
    spawnOnce "volumeicon"

    -- Java/Swing
    setWMName "LG3D"
