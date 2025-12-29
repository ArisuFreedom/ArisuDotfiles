-- Theme.hs
module Theme
  ( -- Colors
    colorBg, colorFg, colorLowWhite, colorBlue, colorCyan
  , colorMagenta, colorRed, colorText, colorYellow
    -- Borders
  , borderNormal, borderFocused
    -- Fonts
  , myFont, myBarFont, myxftFont
    -- Borders
  , myBorderWidth, myNormalBorderColor, myFocusedBorderColor
    -- XPrompt
  , myXPConfig
    -- Tabbed
  , myTabbedTheme
  ) where

import XMonad
import XMonad.Hooks.StatusBar.PP (xmobarColor)
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Shell

-- | Cores principais
colorBg       = "#181616"
colorFg       = "#c5c9c5"
colorLowWhite = "#c8c093"
colorBlue     = "#8ba4b0"
colorCyan     = "#8ea4a2"
colorMagenta  = "#a292a3"
colorRed      = "#c4746e"
colorText     = "#c5c9c5"
colorYellow   = "#c4b28a"

-- | Bordas
borderNormal  = "#0d0c0c"
borderFocused = "#e6c384"

-- | Fonts
myFont     = "JetBrainsMono Nerd Font Mono:size=11"
myBarFont  = "JetBrainsMono Nerd Font Mono-11"
myxftFont  = "xft:JetBrainsMono Nerd Font Mono:size=11"

-- | Borders
myBorderWidth :: Dimension
myBorderWidth = 1
myNormalBorderColor  = borderNormal
myFocusedBorderColor = borderFocused

-- | XPConfig
myXPConfig :: XPConfig
myXPConfig = def
  { font              = myxftFont
  , bgColor           = colorBg
  , fgColor           = colorText
  , bgHLight          = colorBlue
  , fgHLight          = colorBg
  , borderColor       = borderFocused
  , promptBorderWidth = myBorderWidth
  , height            = 22
  , position          = Top
  , historySize       = 256
    -- Ativando fuzzy matching
  , searchPredicate   = fuzzyMatch
  , sorter            = fuzzySort
  }

-- | Tabbed layout theme
myTabbedTheme :: Theme
myTabbedTheme = def
  { fontName            = myxftFont
  , activeColor         = colorBg
  , inactiveColor       = colorBg
  , activeBorderColor   = borderFocused
  , inactiveBorderColor = borderNormal
  , activeTextColor     = colorYellow
  , inactiveTextColor   = colorLowWhite
  , urgentColor         = colorRed
  , urgentBorderColor   = colorRed
  , urgentTextColor     = colorBg
  , decoHeight          = 28
  }

