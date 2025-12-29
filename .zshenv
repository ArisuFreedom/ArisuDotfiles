# zshenv

# Base XDG dirs
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
export XDG_STATE_HOME="${XDG_STATE_HOME:-$HOME/.local/state}"
export XDG_BIN_HOME="${XDG_BIN_HOME:-$HOME/.local/bin}"

# configs
ZDOTDIR="$XDG_CONFIG_HOME/zsh"

# Garante que os dirs existam
mkdir -p "$ZDOTDIR" \
         "$XDG_DATA_HOME/zsh" \
         "$XDG_CACHE_HOME/zsh" \
         "$XDG_STATE_HOME/zsh"

# PATH
export PATH="$XDG_BIN_HOME:$HOME/bin:$PATH"

# Locale
export LANG=C.UTF-8
export LC_ALL=C.UTF-8
export TERM=xterm-256color
[[ "$TERM" == "screen" || "$TERM" == "screen-256color" ]] && TERM=tmux-256color
export COLORTERM=truecolor
