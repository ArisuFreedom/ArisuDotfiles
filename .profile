# ~/.profile
# (POSIX)

[ -n "$__PROFILE_LOADED" ] && return
export __PROFILE_LOADED=1

############################
# XDG Base Directories
############################
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
export XDG_STATE_HOME="${XDG_STATE_HOME:-$HOME/.local/state}"
export XDG_BIN_HOME="${XDG_BIN_HOME:-$HOME/.local/bin}"

# Create directorys
mkdir -p \
  "$XDG_CONFIG_HOME" \
  "$XDG_DATA_HOME" \
  "$XDG_CACHE_HOME" \
  "$XDG_STATE_HOME" \
  "$XDG_BIN_HOME"

############################
# RUNTIME DIR
############################
if [ -z "XDG_RUNTIME_DIR" ]; then
    XDG_RUNTIME_DIR="/tmp/$(id -u)-runtime-dir"

    mkdir -pm 0700 "$XDG_RUNTIME_DIR"
    export XDG_RUNTIME_DIR
fi

############################
# PATH
############################
export PATH="$XDG_BIN_HOME:$PATH"
export PATH="$HOME/.cabal/bin:$PATH"

############################
# Locale
############################
export LANG="C.UTF-8"
export LC_ALL="C.UTF-8"

############################
# Editor, Pager, Browser
############################
export EDITOR="emacsclient -t -a ''"
export VISUAL="$EDITOR"
export PAGER="less"
export LESS="-R"
export LESSHISTFILE="-"
export MANPAGER="sh -c 'col -bx | bat -l man -p'"
export BROWSER="librewolf"

############################
# FZF Colorscheme
############################
export FZF_DEFAULT_OPTS="--color=fg:#cdcdcd --color=bg:#141415 --color=hl:#f3be7c --color=fg+:#aeaed1 --color=bg+:#252530 --color=hl+:#f3be7c --color=border:#606079 --color=header:#6e94b2 --color=gutter:#141415 --color=spinner:#7fa563 --color=info:#f3be7c --color=pointer:#aeaed1 --color=marker:#d8647e --color=prompt:#bb9dbd"

############################
# Java / GPG / Ripgrep
############################
export _JAVA_OPTIONS="-Djava.util.prefs.userRoot=$XDG_CONFIG_HOME/java"
export GNUPGHOME="$XDG_DATA_HOME/gnupg"
export RIPGREP_CONFIG_PATH="$XDG_CONFIG_HOME/ripgrep/config"
export XINITRC="$XDG_CONFIG_HOME"/X11/xinitrc
export XSERVERRC="$XDG_CONFIG_HOME"/X11/xserverrc
export XAUTHORITY="$XDG_RUNTIME_DIR"/Xauthority

############################
# Terminal colors
############################
export COLORTERM="truecolor"

# tmux (POSIX)
if [ "$TERM" = "tmux" ]; then
  export TERM="tmux-256color"
fi


[ -f "/home/arch/.ghcup/env" ] && . "/home/arch/.ghcup/env" # ghcup-env