module Vars
  ( attach
  , attachVim
  , browser
  , discord
  , fileManager
  , vim
  , emacsclient
  , locker
  , launcher
  , modMask'
  , screenshot
  , terminal'
  , tmux'
  , trayer
  , onion
  , vimux
  ) where

import XMonad

browser :: String
browser = "librewolf"

onion :: String
onion = "torbrowser"

discord :: String
discord = "discord-ptb"

terminal' :: String
terminal' = "alacritty"

fileManager :: String
fileManager = terminal' ++ " -e sfm"

vim :: String
vim = terminal' ++ " -e vim"

emacsclient :: String
emacsclient = "emacsclient -a '' -nc"

locker :: String
locker = "slock"

launcher :: String
launcher = "dmenu_run -c -l 20"

modMask' :: KeyMask
modMask' = mod4Mask

screenshot :: String
screenshot = "maim -s -u | xclip -selection clipboard -t image/png"

tmux' :: String
tmux' = "tmux new-session -d -s 'default'"

attach :: String
attach = terminal' ++ " -e tmux attach -t default"

attachVim :: String
attachVim = terminal' ++ " -e tmux attach -t vim"

trayer :: String

trayer = "trayer \
  \--edge top \
  \--align right \
  \--widthtype request \
  \--padding 8 \
  \--SetDockType true \
  \--SetPartialStrut true \
  \--expand true \
  \--height 22 \
  \--transparent true \
  \--tint 0x16161D \
  \--alpha 0"


vimux :: String
vimux = "tmux new-session -d -s 'vim' 'vim'"

