# zshrc

# load Colors
autoload -U colors && colors

# load completions
autoload -Uz compinit
if [[ -n ~/.zcompdump(#qN.mh+24) ]]; then
  compinit
else
  compinit -C
fi

# Interactive
[[ $- != *i* ]] && return

# History
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

# completions cache
[ -d "$XDG_CACHE_HOME/zsh" ] || mkdir -p "$XDG_CACHE_HOME/zsh"
zstyle ':completion:*' cache-path "$XDG_CACHE_HOME/zsh/zcompcache"
compinit -d "$XDG_CACHE_HOME/zsh/zcompdump-$ZSH_VERSION"

# Cool Opts
setopt autocd # just type the directory end press enter
setopt auto_param_slash # when a dir is completed, add a / instead of a trailing space
setopt no_case_glob no_case_match # make cmp case insensitive
setopt globdots # include dotfiles
setopt extended_glob # match ~ # ^
setopt interactive_comments # allow comments in interactive shell
stty stop undef # disable accidental ctrl s

# Aliases
alias c='clear'
alias vim='nvim'
alias clip='xclip -selection clipboard'
alias df='df -h'
alias du='du -h'
alias ls='ls --color'
alias la='ls -a --color'
alias ll='ls -l --color'
alias llh='ls -alh --color'
alias mkdir='mkdir -p'
alias rm='rm -f'
alias startx='startx "$XDG_CONFIG_HOME/X11/xinitrc"'
#alias sudo='doas'
alias wget="wget --hsts-file='$XDG_CACHE_HOME/wget-hsts'"
alias nohup='nohup > ~/.local/state/nohup/nohup.out 2>&1'

# Open buffer line in editor
autoload -Uz edit-command-line
zle -N edit-command-line

# Edit mode
bindkey -e
bindkey '^L' clear-screen
bindkey '^p' history-search-backward
bindkey '^n' history-search-forward
bindkey '^x^e' edit-command-line
bindkey ' ' magic-space

# evals
eval "$(fzf --zsh)"
eval "$(starship init zsh)"


[ -f "/home/arch/.local/share/ghcup/env" ] && . "/home/arch/.local/share/ghcup/env" # ghcup-env