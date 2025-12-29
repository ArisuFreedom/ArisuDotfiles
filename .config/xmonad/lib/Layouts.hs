-- Layouts.hs
module Layouts
  ( myLayoutHook
  ) where

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid
import XMonad.Layout.Tabbed
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.ToggleLayouts

import Theme

-- | Smart spacing: only applies gaps when more than one window exists
mySmartSpacing n = smartSpacing n

-- | Main layout
myLayoutHook =
  avoidStruts $
  smartBorders $
  toggleLayouts full tiled
  ||| tab

  where
    -- Master area configuration
    nmaster = 1
    ratio   = 1/2
    delta   = 3/100

    -- Tiled layout with tabs at the bottom
    tiled =
      renamed [Replace "\984434"] $
      mySmartSpacing 5 $
      addTabsBottom shrinkText myTabbedTheme $
      ResizableTall nmaster delta ratio []

    -- Tabbed-only layout
    tab =
      renamed [Replace "TAB"] $
      tabbedBottomAlways shrinkText myTabbedTheme

    -- Fullscreen layout without borders
    full =
      renamed [Replace "\983699"] $
      noBorders Full
