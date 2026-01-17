module StatusBar (mySB
         , myPP
      -- , toggleStrutsKey
         ) where

import Theme
import XMonad
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Util.ClickableWorkspaces
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad (scratchpadWorkspaceTag)

myPP :: PP
myPP = (filterOutWsPP [scratchpadWorkspaceTag] def)
  { ppSep             = magenta " | "
  , ppTitleSanitize   = const "" -- xmobarStrip
  , ppCurrent         = wrap " " "" . xmobarBorder "Top" colorBlue 2
  , ppHidden          = white . wrap " " ""
  , ppHiddenNoWindows = lowWhite . wrap " " ""
  , ppUrgent          = red . wrap (yellow "!") (yellow "!")
  , ppOrder           = \(ws:l:t:xs) -> [ws, l] -- \[ws, l, _, wins] -> [ws, l, wins]
  , ppExtras          = [] -- [logTitles formatFocused formatUnfocused]
  }
  where
    -- Funções de formatação para janela focada e não focada
    formatFocused   = wrap (white "[") (white "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue . ppWindow

    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    -- Colorscheme
    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor colorMagenta  ""
    blue     = xmobarColor colorBlue     ""
    white    = xmobarColor colorFg       ""
    yellow   = xmobarColor colorYellow   ""
    red      = xmobarColor colorRed      ""
    lowWhite = xmobarColor colorLowWhite ""


mySB :: StatusBarConfig
mySB = statusBarProp
  "xmobar -x0 ~/.config/xmobar/xmobar.hs"
  (clickablePP myPP)

-- toggleStrutsKey :: XConfig l -> (KeyMask, KeySym)
-- toggleStrutsKey XConfig { modMask = m } = (m, xK_b)
