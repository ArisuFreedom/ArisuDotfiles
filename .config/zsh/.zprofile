# zprofile

# Editor e ferramentas
export EDITOR="emacsclient -t -a ''"
export VISUAL="$EDITOR"
export PAGER="less"
export MANPAGER="sh -c 'col -bx | bat -l man -p'"
export LESS='-R'
export LESSHISTFILE='-'
export BROWSER="librewolf"
export _JAVA_OPTIONS="-Djava.util.prefs.userRoot=$XDG_CONFIG_HOME/java"
export GNUPGHOME="$XDG_DATA_HOME/gnupg"
export RIPGREP_CONFIG_PATH="$XDG_CONFIG_HOME/ripgrep/config"

