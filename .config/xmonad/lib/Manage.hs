-- Manage.hs
module Manage
  ( myManageHook
  ) where

import XMonad
import XMonad.StackSet as W
import XMonad.Hooks.ManageHelpers
import Data.List (isInfixOf)

-- Função para pegar o nome da janela
name' = stringProperty "WM_NAME"

-- | Janelas que devem ser ignoradas (não gerenciadas pelo XMonad)
myIgnores :: [String]
myIgnores =  ["trayer","stalonetray"] -- ex: ["trayer","stalonetray"]

-- | Janelas que devem flutuar
myFloats :: [String]
myFloats = ["feh"
          , "xmessage"
          , "scratchpad"
          ]

-- | Regras de shift por classe de janela
myClassShifts :: [(String, String)]
myClassShifts =
  [ ("discord-ptb"  , "3"  )  -- irc → 話
  , ("discord"      , "3"  )
  , ("firefox-esr"  , "2"  )  -- www → 電
  , ("librewolf"    , "2"  )
  , ("tor-browser"  , "2"  )
  , ("qutebrowser"  , "2"  )
  , ("spotify"      , "5"  )  -- music → 音
  , ("youtube-music", "5"  )
  , ("PrismLauncher", "4"  )  -- games → 遊
  , ("Vim"          , "1"  )  -- doom → 闇
  , ("nvim"         , "1"  )
  , ("Gvim"         , "1"  )
  , ("emacs"        , "1"  )
  , ("vis"          , "1"  )
  , ("vi"           , "1"  )
  ]

-- | Regras de flutuação por nome
myNames :: [String]  = ["scratchpad"]

-- | ManageHook principal
myManageHook :: ManageHook
myManageHook = composeAll . concat $
  [ [ resource  =? r --> doIgnore           | r       <- myIgnores     ]
  , [ className =? c --> doShift ws         | (c, ws) <- myClassShifts ]
  , [ className =? c --> doCenterFloat      | c       <- myFloats      ]
  , [ name'     =? n --> doCenterFloat      | n       <- myNames       ]
  , [ isDialog       --> doFloat                                       ]
  , [ className >>= \c -> if "Minecraft" `isInfixOf` c then doShift "4" else mempty ]
  ]
