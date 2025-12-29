# zshrc

ZINIT_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"
[ ! -d $ZINIT_HOME ] && mkdir -p "$(dirname $ZINIT_HOME)"
[ ! -d $ZINIT_HOME/.git ] && git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT_HOME"

# source zinit plugin manager
source "${ZINIT_HOME}/zinit.zsh"

autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# zsh plugins
zinit light zsh-users/zsh-syntax-highlighting
zinit light zsh-users/zsh-completions
zinit light zsh-users/zsh-autosuggestions
zinit light Aloxaf/fzf-tab

# load completions
autoload -U compinit && compinit

zinit cdreplay -q

# só carrega se for interativo
[[ $- != *i* ]] && return

# histórico
[ -d "$XDG_STATE_HOME/zsh" ] || mkdir -p "$XDG_STATE_HOME/zsh"
HISTFILE="$XDG_STATE_HOME/zsh/history"
SAVEHIST=10000
HISTSIZE=5000
HISTDUP=erase
setopt append_history
setopt share_history
setopt inc_append_history
setopt hist_ignore_space
setopt hist_ignore_all_dups
setopt hist_save_no_dups
setopt hist_ignore_dups
setopt hist_find_no_dups
export HISTORY_IGNORE="(ls|cd|pwd|exit|sudo reboot|history|cd -|cd ..)"

# Completion customization
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' menu no
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'eza --color=always --group-directories-first --icons --git $realpath'
zstyle ':fzf-tab:complete:__zoxide_z:*' fzf-preview 'eza --color=always --group-directories-first --icons --git $realpath'

# completions cache
[ -d "$XDG_CACHE_HOME/zsh" ] || mkdir -p "$XDG_CACHE_HOME/zsh"
zstyle ':completion:*' cache-path "$XDG_CACHE_HOME/zsh/zcompcache"
compinit -d "$XDG_CACHE_HOME/zsh/zcompdump-$ZSH_VERSION"

# Aliases
alias startx='startx "$XDG_CONFIG_HOME/X11/xinitrc"'
alias ls='eza -l --color=always --group-directories-first --icons --git'
alias lla='eza -al --color=always --group-directories-first --icons --git'
alias la='eza -a --color=always --group-directories-first --icons --git'
alias ll='eza -l --color=always --group-directories-first --icons --git'
alias lt='eza -aT --color=always --group-directories-first --icons --git'
alias df='df -h'
alias du='du -h'
alias mkdir='mkdir -p'
alias sudo='doas'
alias rm='rm -f'
alias rmdir='rm -rf'
alias c='clear'
alias wget="wget --hsts-file='$XDG_CACHE_HOME/wget-hsts'"
alias clip='xclip -selection clipboard'

# Edit mode
bindkey -e
bindkey '^L' clear-screen
bindkey '^p' history-search-backward
bindkey '^n' history-search-forward

# Funções
[[ -f "$ZDOTDIR/functions.zsh" ]] && source "$ZDOTDIR/functions.zsh"

# evals
eval "$(zoxide init zsh --hook prompt --cmd cd)"
eval "$(fzf --zsh)"
eval "$(starship init zsh)"
