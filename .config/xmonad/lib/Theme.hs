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

-- | Colors
colorBg       = "#141415" -- background
colorFg       = "#cdcdcd" -- foreground
colorText     = "#cdcdcd"

colorLowWhite = "#aeaed1" -- cyan suave (normal.white/cyan vibe)
colorBlue     = "#6e94b2"
colorCyan     = "#aeaed1"
colorMagenta  = "#bb9dbd"
colorRed      = "#d8647e"
colorYellow   = "#f3be7c"

-- | Borders
borderNormal  = "#252530" -- normal.black
--borderFocused = "#f3be7c" -- yellow (destaque)
borderFocused = colorBlue

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

