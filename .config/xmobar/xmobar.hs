{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main (main) where

import Xmobar
import System.Directory

--------------------------------------------------------------------------------
-- CORES (declarativas)
--------------------------------------------------------------------------------

colorBg       = "#181616"
colorFg       = "#c5c9c5"
colorLowWhite = "#c8c093"
colorBlue     = "#8ba4b0"
colorCyan     = "#8ea4a2"
colorMagenta  = "#a292a3"
colorRed      = "#c4746e"
colorYellow   = "#c4b28a"

--------------------------------------------------------------------------------
-- DISPOSITIVOS (declarativos)
--------------------------------------------------------------------------------

wifiDevice     = "/sys/class/net/wlan0/"
wifiIface      = "wlan0"

ethernetDevice = "/sys/class/net/enp34s0/"
ethernetIface  = "enp34s0"

batteryDevice  = "/sys/class/power_supply/BAT0"
batteryName    = "BAT0"

cpuTempDevice  = "/sys/class/hwmon/hwmon/temp1_input"
cpuTempLabel   = "coretemp"

--------------------------------------------------------------------------------
-- MAIN
--------------------------------------------------------------------------------

main :: IO ()
main = xmobar =<< configFromArgs =<< myConfig

--------------------------------------------------------------------------------
--  MAIN DYNAMIC CONFIG
--------------------------------------------------------------------------------

myConfig :: IO Config
myConfig = do

    hasWifi     <- doesPathExist wifiDevice
    hasEthernet <- doesPathExist ethernetDevice
    hasBattery  <- doesPathExist batteryDevice
    hasCPUTemp  <- doesPathExist cpuTempDevice

    let netCmds =
            [ Run $ Network wifiIface
                ["-t", "\62473 <fc=" ++ colorBlue ++ "><rx>KB</fc> | \62474 <fc="
                       ++ colorBlue ++ "><tx>KB</fc>"]
                10
            | hasWifi ] <>
            [ Run $ Network ethernetIface
                ["-t", "\62473 <fc=" ++ colorBlue ++ "><rx>KB</fc> | \62474 <fc="
                       ++ colorBlue ++ "><tx>KB</fc>"]
                10
            | hasEthernet ]

    let batCmds =
            [ Run $ BatteryN [batteryName]
                [ "-t", "<fc=" ++ colorYellow ++ ">Bat <left>% <watts>W</fc>"
                , "-L", "5"
                , "-H", "75"
                , "--low", colorLowWhite
                , "--normal", colorYellow
                , "--high", colorRed
                ]
                50 batteryName
            | hasBattery ]

    let coreCmd =
            [ Run $ CoreTemp ["-t","\62153 <fc=" ++ colorRed ++ "><core0>C</fc>"] 50
            | hasCPUTemp ]

    let templateParts =
            [ "%UnsafeXMonadLog% }{ "
            , "%multicpu% / "
            , "%memory% / "
            , "%date% / "
            ]
            ++ [ "%wlan0% / "    | hasWifi     ]
            ++ [ "%enp34s0% / "  | hasEthernet ]
            ++ [ "%BAT0% / "     | hasBattery  ]
            ++ [ "%coretemp% / " | hasCPUTemp  ]
            ++ [ "%_XMONAD_TRAYPAD%"           ]

    pure baseConfig
        { template = concat templateParts
        , commands =
            [ Run UnsafeXMonadLog
            , Run $ Date
                ("<fc=" ++ colorBlue ++ ">(%H:%M)</fc> - <fc=" ++ colorBlue ++ ">%b %d %Y</fc>")
                "date" 10
            , Run $ MultiCpu
                ["-t","<fn=1>\62652</fn> <fc=" ++ colorLowWhite ++ "><total>%</fc>"]
                10
            , Run $ Memory
                ["-t","<fn=1>\61381</fn> <fc=" ++ colorLowWhite ++ "><usedratio>%</fc>"]
                10
            , Run $ XPropertyLog "_XMONAD_TRAYPAD"
            ]
            <> netCmds
            <> batCmds
            <> coreCmd
        }

--------------------------------------------------------------------------------
-- BASE CONFIG
--------------------------------------------------------------------------------

baseConfig :: Config
baseConfig = defaultConfig

    { font            =   "JetBrainsMono Nerd Font Mono 10"
    , additionalFonts = [ "JetBrainsMono Nerd Font Mono 18" ]
    , bgColor         = colorBg
    , fgColor         = colorFg
    , border          = BottomB
    , borderColor     = colorYellow
    , position        = TopSize L 100 26
    , lowerOnStart    = True
    , hideOnStart     = False
    , allDesktops     = True
    , persistent      = True
    , iconRoot        = "~/.config/xmonad/xpm/"
    , alpha           = 255
    , alignSep        = "}{"
    }
