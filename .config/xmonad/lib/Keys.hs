-- Keys.hs
module Keys
  ( myKeyBindings
  ) where

import Scratchpads (myScratchPads)
import System.Exit (exitWith, ExitCode(ExitSuccess))
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.TopicSpace
import XMonad.Hooks.ManageDocks (ToggleStruts(..))
import XMonad.Layout.ToggleLayouts (ToggleLayout(Toggle))
import XMonad.Prompt
import XMonad.Prompt (XPConfig(..), mkXPrompt)
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Man
import XMonad.Prompt.Shell
import XMonad.Prompt.Shell
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W

import Topic   -- seu módulo Topic.hs
import Layouts -- para referência a layouts, se necessário
import Startup -- para apps
import Theme   -- para e cores, se precisar
import Vars

-- | Atalhos principais
myKeyBindings :: [(String, X ())] =

    -- XMonad
    [ ("M-q"        , spawn "xmonad --recompile && xmonad --restart" )
    , ("M-S-q"      , io (exitWith ExitSuccess)                      )
    , ("M-b"        , sendMessage ToggleStruts                       )

    -- Prompts
    , ("M-?"        , manPrompt   myXPConfig )
    , ("M-S-p"      , shellPrompt myXPConfig )

    -- Apps favoritos
    , ("M-C-s"      , spawn screenshot  )
    , ("M-S-a"      , spawn browser     )
    , ("M-S-e"      , spawn emacsclient )
    , ("M-S-l"      , spawn locker      )
    , ("M-S-o"      , spawn onion       )
    , ("M-S-v"      , spawn vim         )
    , ("M-S-w"      , spawn fileManager )
    , ("M-d"        , spawn discord     )
    , ("M-x"        , spawn launcher    )

    -- ScratchPads
    , ("M-<Return>" , namedScratchpadAction myScratchPads "terminal"      )
    , ("M-e"        , namedScratchpadAction myScratchPads "emacs-scratch" )

    -- Volume
    , ("<Print>"               , spawn screenshot  )
    , ("<XF86AudioLowerVolume>", spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"  )
    , ("<XF86AudioMute>"       , spawn "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle" )
    , ("<XF86AudioRaiseVolume>", spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+"  )

    -- Workspace navigation
    , ("M-C-d", prevWS)
    , ("M-C-u", nextWS)

    -- Layouts
    , ("M-f", sendMessage ToggleStruts >> sendMessage (Toggle "\983699") )
    , ("M-t", withFocused $ windows . W.sink                             )

    -- TopicSpace
    , ("M-<Tab>", toggleTopic   )
    ]
  ++
  -- Atalhos numéricos para workspaces (1-7)
  [ ("M-" ++ show n, goto t)
  | (n, t) <- zip [1..7] (topicNames topicItems)
  ]
  ++
  [ ("M-S-" ++ show n, windows $ W.shift t)
  | (n, t) <- zip [1..7] (topicNames topicItems)
  ]
