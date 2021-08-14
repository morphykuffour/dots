# . "$HOME/.cargo/env"

# Most ${ENV_VAR} variables should be saved here.

export MANPATH="/usr/local/man:$MANPATH"

# XDG_Base_Directory_support
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"

# System directories
export XDG_DATA_DIRS="/usr/local/share:/usr/share"
export XDG_CONFIG_DIRS="/etc/xdg"

# Options
export LANG=en_US.UTF-8
export LC_PAPER=en_US.utf8
export EDITOR='nvim'
export SVN_EDITOR='nvim'

# Compilation flags
export ARCHFLAGS="-arch x86_64"

# SSH
export SSH_KEY_PATH="~/.ssh/id_ed25519"

# export TERMINAL="termite"
export TERM="xterm-256color"

# zsh options
export ZDOTDIR=$HOME/.config/zsh/

# Create a cache folder if it isn't exists
# Define a custom file for compdump
if [ ! -d "$HOME/.cache/zsh" ]; then
    mkdir -p $HOME/.cache/zsh
fi
export ZSH_COMPDUMP="$HOME/.cache/zsh/zcompdump-$HOST-$ZSH_VERSION"

export fpath=(~/.config/zsh/completions/ $fpath)

if [[ $s(command -v rg) ]]; then
    export FZF_DEFAULT_COMMAND='rg --hidden --ignore .git -g ""'
fi

# Determine if we are an SSH connection
if [[ -n "$SSH_CLIENT" ]] || [[ -n "$SSH_TTY" ]]; then
    export IS_SSH=true
else
    case $(ps -o comm= -p $PPID) in
        sshd|*/sshd) IS_SSH=true
    esac
fi


