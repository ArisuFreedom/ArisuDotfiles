-- Topic.hs
module Topic
  ( topicItems
  , topicNames
  , myTopicConfig
  , goto
  , promptedGoto
  , promptedShift
  , toggleTopic
  ) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Prompt
import XMonad.Prompt.Workspace
import XMonad.Actions.TopicSpace
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run

import Vars

-- | Diretórios padrão por tópico (para spawn de apps)
mySysDir, myWebDir, myVimDir, myChatDir, myMusicDir, myGamesDir, myMiscDir :: String
mySysDir   = "."
myWebDir   = "."
myVimDir   = "."
myChatDir  = "."
myMusicDir = "."
myGamesDir = "."
myMiscDir  = "."

-- | Tópicos com ações
topicItems :: [TopicItem]
topicItems =
  [ TI "1" mySysDir    (pure ())
  , TI "2" myWebDir    (pure ())
  , TI "3" myVimDir    (pure ())
  , TI "4" myChatDir   (pure ())
  , TI "5" myMusicDir  (pure ())
  , TI "6" myGamesDir  (pure ())
  , TI "7" myMiscDir   (pure ())
  ]

-- | Lista de nomes de workspaces
myTopicNames :: [String]
myTopicNames = map tiName topicItems

-- | Configuração do TopicSpace
myTopicConfig :: TopicConfig
myTopicConfig = def
  { topicDirs          = tiDirs topicItems
  , topicActions       = tiActions topicItems
  , defaultTopicAction = const (pure ())
  , defaultTopic       = "1"
  }

-- | Funções de navegação
goto :: Topic -> X ()
goto = switchTopic myTopicConfig

promptedGoto :: X ()
promptedGoto = workspacePrompt def goto

promptedShift :: X ()
promptedShift = workspacePrompt def $ windows . W.shift

toggleTopic :: X ()
toggleTopic = switchNthLastFocusedByScreen myTopicConfig 1
