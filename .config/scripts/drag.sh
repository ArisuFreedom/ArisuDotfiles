#!/bin/sh

doas mkdir -p /etc/libinput
doas tee /etc/libinput/local-overrides.quirks >/dev/null <<ENDHERE
[Never Debounce]
MatchUdevType=mouse
ModelBouncingKeys=1
ENDHERE
