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
# PATH
############################
export PATH="$XDG_BIN_HOME:$PATH"

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
# Java / GPG / Ripgrep
############################
export _JAVA_OPTIONS="-Djava.util.prefs.userRoot=$XDG_CONFIG_HOME/java"
export GNUPGHOME="$XDG_DATA_HOME/gnupg"
export RIPGREP_CONFIG_PATH="$XDG_CONFIG_HOME/ripgrep/config"

############################
# Terminal colors
############################
export COLORTERM="truecolor"

# tmux (POSIX)
if [ "$TERM" = "tmux" ]; then
  export TERM="tmux-256color"
fi
