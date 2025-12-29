module Scratchpads (myScratchPads) where

import XMonad
import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W

import Vars

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "emacs-scratch"    emacsCmd findEmacs manageEmacs
                ]
  where
    spawnTerm  = terminal' ++ " --class=AlacrittyScratch,AlacrittyScratch"
    findTerm   = resource =? "AlacrittyScratch"
    manageTerm = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 - h
                 l = 0.95 - w

    emacsCmd    = "emacsclient -a '' -nc -F '(quote (name . \"emacs-scratch\"))'"
    findEmacs   = title =? "emacs-scratch"
    manageEmacs = customFloating $ W.RationalRect l t w h
                where
                  h = 0.95
                  w = 0.95
                  t = 0.975 - h
                  l = 0.975 - w
