{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main (main) where

import Xmobar

--------------------------------------------------------------------------------
-- COLORS
--------------------------------------------------------------------------------

colorBg       = "#141415"
colorFg       = "#cdcdcd"
colorBlue     = "#6e94b2"
colorRed      = "#d8647e"
colorYellow   = "#f3be7c"
colorLowWhite = "#aeaed1"
colorGray     = "#606060" -- Para os colchetes e separadores

-- Helpers de cores para limpar o código
blue   = xmobarColor colorBlue ""
red    = xmobarColor colorRed ""
yellow = xmobarColor colorYellow ""
gray   = xmobarColor colorGray ""

--------------------------------------------------------------------------------
-- ICONS
--------------------------------------------------------------------------------

-- Helper para usar a fonte de ícones (index 1 nas additionalFonts)
ico m    = "<fn=1>" ++ m ++ "</fn>"

cpuIco   = ico "\xf2db"
memIco   = ico "\xf0c9"
netIco   = ico "\xf012"
dateIco  = ico "\xf017"
batOn    = ico "\xf1e6"
batOff   = ico "\xf242"
volIco   = ico "\xf028" -- fa-volume-high

--------------------------------------------------------------------------------
-- HELPERS
--------------------------------------------------------------------------------

xmobarColor fg bg = wrap ("<fc=" ++ fg ++ (if null bg then "" else "," ++ bg) ++ ">") "</fc>"
wrap l r m = if null m then "" else l ++ m ++ r
inSquare m = gray "[" ++ m ++ gray "]"

-- Time Helpers (ex: 5s instead 50)
s n = n * 10
m n = n * 600

--------------------------------------------------------------------------------
-- DEVICES
--------------------------------------------------------------------------------

ethernetIface = "enp34s0"
batteryName   = "BAT0"

--------------------------------------------------------------------------------
-- MAIN
--------------------------------------------------------------------------------

main :: IO ()
main = xmobar =<< configFromArgs myConfig

myConfig :: Config
myConfig = baseConfig
    { -- Template
      template = " " ++ inSquare (blue dateIco ++ " %clock%") ++ " "
                     ++ inSquare (blue dateIco ++ " %calendar%") ++ " "
                     ++ inSquare (blue netIco ++ " %enp34s0%")
                     ++ " } %UnsafeXMonadLog% { "
                     ++ inSquare (blue cpuIco ++ " %cpu%") ++ " "
                     ++ inSquare (blue memIco ++ " %memory%") ++ " "
                     ++ inSquare (blue "%volume%") ++ " "
                     ++ "%_XMONAD_TRAYPAD%"

    , commands =
        [ Run UnsafeXMonadLog

        , Run $ Date (blue "%H:%M") "clock" (s 1)

        , Run $ Date (blue "%a, %d %b %Y") "calendar" (m 1)


        , Run $ Cpu
            [ "-t", "<bar>" -- <total> for percent
            , "--bback", "━"
            , "--bfore", "▬"
            , "--bwidth", "10"
            , "-L", "30", "-H", "75"
            , "--low", colorFg, "--normal", colorYellow, "--high", colorRed
            , "--suffix", "True" -- Adiciona % automático
            , "--ppad", "2"     -- Evita que a barra "pule" ao mudar dígitos
            ] (s 1)

        , Run $ Memory
            [ "-t", "<usedbar>" -- <usedratio> for percent
            , "--bback", "━"
            , "--bfore", "▬"
            , "--bwidth", "10"
            , "-L", "30", "-H", "75"
            , "--low", colorFg, "--normal", colorYellow, "--high", colorRed
            , "--suffix", "True"
            , "--ppad", "2"
            ] (s 1)

        , Run $ Network ethernetIface
            [ "-t", blue "<rx>KB" ++ gray "|" ++ blue "<tx>KB"
            ] (s 1)

        , Run $ Com "sh"
            [ "-c"
            , "v=$(pactl get-sink-volume @DEFAULT_SINK@ | awk '{print $5}' | tr -d '%'); n=$((v/10)); printf '%s %s' \"$v%\" \"" ++ volIco ++ " $(printf '▬%.0s' $(seq 1 $n))$(printf '━%.0s' $(seq 1 $((10-n))))\""
            ]
            "volume"
            (s 1)

        , Run $ Battery
            [ "-t", "<acstatus> <left>"
            , "-L", "20", "-H", "80"
            , "--low", colorRed, "--normal", colorYellow, "--high", colorBlue
            , "--suffix", "True"
            , "--"
            , "--off", batOff ++ " "
            , "--on",  blue batOn ++ " "
            , "--idle", blue batOn ++ " "
            ] (s 5)

        , Run $ XPropertyLog "_XMONAD_TRAYPAD"
        ]
    }

--------------------------------------------------------------------------------
-- BASE CONFIG
--------------------------------------------------------------------------------

baseConfig :: Config
baseConfig = defaultConfig
    { font            = "Iosevka Nerd Font Mono 11"
    , additionalFonts = [ "Iosevka Nerd Font Mono 14" ]
    , bgColor         = colorBg
    , fgColor         = colorFg
    , border          = BottomB
    , borderColor     = colorBlue
    , position        = TopSize L 100 24
    , allDesktops     = True
    , persistent      = True
    , alpha           = 255
    , alignSep        = "}{"
    }
