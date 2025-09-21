#!/bin/bash

gentoo-pipewire-launcher &

xrandr --output DisplayPort-1 --mode 1920x1080 --rate 100

exec dbus-launch --sh-syntax --exit-with-session dwm

