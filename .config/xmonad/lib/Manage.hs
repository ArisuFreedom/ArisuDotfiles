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
  [ ("discord-ptb"  , "話"  )  -- irc → 話
  , ("discord"      , "話"  )
  , ("firefox-esr"  , "電"  )  -- www → 電
  , ("librewolf"    , "電"  )
  , ("tor-browser"  , "電"  )
  , ("qutebrowser"  , "電"  )
  , ("spotify"      , "音"  )  -- music → 音
  , ("youtube-music", "音"  )
  , ("PrismLauncher", "遊"  )  -- games → 遊
  , ("Vim"          , "闇"  )  -- doom → 闇
  , ("nvim"         , "闇"  )
  , ("Gvim"         , "闇"  )
  , ("emacs"        , "闇"  )
  , ("vis"          , "闇"  )
  , ("vi"           , "闇"  )
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
  , [ className >>= \c -> if "Minecraft" `isInfixOf` c then doShift "遊" else mempty ]
  ]
