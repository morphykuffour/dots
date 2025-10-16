#!/usr/bin/env zsh
# kitty integration
if [[ -n $KITTY_INSTALLATION_DIR ]]; then
    export KITTY_SHELL_INTEGRATION="enabled"
    autoload -Uz -- "$KITTY_INSTALLATION_DIR"/shell-integration/zsh/kitty-integration
    kitty-integration
    unfunction kitty-integration
fi

# --------------------------------- SETTINGS ----------------------------------
setopt AUTO_CD
setopt BEEP
#setopt CORRECT
setopt HIST_BEEP
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_DUPS
setopt HIST_REDUCE_BLANKS
setopt HIST_SAVE_NO_DUPS
setopt HIST_VERIFY
setopt INC_APPEND_HISTORY
setopt INTERACTIVE_COMMENTS
setopt MAGIC_EQUAL_SUBST
setopt NO_NO_MATCH
setopt NOTIFY
setopt NUMERIC_GLOB_SORT
setopt PROMPT_SUBST
setopt SHARE_HISTORY

# -------------------------------- HISTORY ------------------------------------
HISTFILE=~/.zsh_history
HIST_STAMPS=mm/dd/yyyy
HISTSIZE=10000000
SAVEHIST=10000000
ZLE_RPROMPT_INDENT=0
WORDCHARS=${WORDCHARS//\/}
PROMPT_EOL_MARK=
TIMEFMT=$'\nreal\t%E\nuser\t%U\nsys\t%S\ncpu\t%P'

# ------------------------------ ZSH completion system ------------------------
autoload -Uz compinit
compinit -d ~/.cache/zsh/zcompdump
compdef _gnu_generic delta
# compdef _qmk qmk
_comp_options+=(globdots)		# Include hidden files.
zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' rehash true
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'
zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' '+l:|=* r:|=*'

# ----------------------------------- MISC -----------------------------------
# enable terminal linewrap
setterm -linewrap on 2> /dev/null

# If this is an xterm set the title to user@host:dir
case "$TERM" in
    xterm*|rxvt*|Eterm|aterm|kterm|gnome*|alacritty)
        precmd() { print -Pnr -- $'\e]0;%n@%m: %~\a' }
        ;;
esac

# Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)# Include hidden files.

#  ---------------------------- Key bindings -------------------------------------
# vi mode
bindkey -v
export KEYTIMEOUT=1

# fix cursor
# Change cursor shape for different vi modes.
function zle-keymap-select {
  if [[ ${KEYMAP} == vicmd ]] ||
     [[ $1 = 'block' ]]; then
    echo -ne '\e[1 q'
  elif [[ ${KEYMAP} == main ]] ||
       [[ ${KEYMAP} == viins ]] ||
       [[ ${KEYMAP} = '' ]] ||
       [[ $1 = 'beam' ]]; then
    echo -ne '\e[5 q'
  fi
}
zle -N zle-keymap-select
zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
    echo -ne "\e[5 q"
}
zle -N zle-line-init
echo -ne '\e[5 q' # Use beam shape cursor on startup.
preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.

# Edit line in vim with ctrl-e:
autoload edit-command-line
zle -N edit-command-line


export VI_MODE_SET_CURSOR=true

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -M menuselect '/' vi-history-search-backward
bindkey -M vicmd '^e' edit-command-line
bindkey '^e' edit-command-line

bindkey -v '^?' backward-delete-char
bindkey '^U' backward-kill-line
bindkey '^[[2~' overwrite-mode
bindkey '^[[3~' delete-char
bindkey '^[[H' beginning-of-line
bindkey '^[[1~' beginning-of-line
bindkey '^[[F' end-of-line
bindkey '^[[4~' end-of-line
bindkey '^[[1;5C' forward-word
bindkey '^[[1;5D' backward-word
bindkey '^[[3;5~' kill-word
bindkey '^[[5~' beginning-of-buffer-or-history
bindkey '^[[6~' end-of-buffer-or-history
bindkey '^[[Z' undo
bindkey ' ' magic-space



# xplr
# function xcd() {
#   cd "$(xplr --print-pwd-as-result)"
# }

# ls after cd
# chpwd() {
#     exa
# }

# alias xcd=ii'cd "$(xplr --print-pwd-as-result)"'
# bindkey -s '^q' 'xcd^M'

bindkey '^[[P' delete-char

bindkey -s '^z' 'fg\n'

zstyle ':fzf-tab:complete:*:*' fzf-preview 'less ${(Q)realpath}'
export LESSOPEN='| ~/.local/bin/lessfilter %s'
zstyle ':completion:*:git-checkout:*' sort false
zstyle ':completion:*:descriptions' format '[%d]'
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'ls -1 --color=always $realpath'
zstyle ':fzf-tab:*' switch-group ',' '.'

# WSL environment
if [[ -n "$IS_WSL" || -n "$WSL_DISTRO_NAME" ]]; then
    export BROWSER="/mnt/c/Program Files/BraveSoftware/Brave-Browser/Application/brave.exe"
else
    export BROWSER="brave"
fi

# open vscode from terminal in Mac OS
case "$(uname -s)" in
  Darwin)
    [ -f /opt/homebrew/etc/profile.d/autojump.sh ] && . /opt/homebrew/etc/profile.d/autojump.sh
    code () { VSCODE_CWD="$PWD" open -n -b "com.microsoft.VSCode" --args $* ;}
    alias o="open"
    ;;
  Linux)
    source $HOME/.zsh/completions/autojump.zsh
    # source $HOME/.zsh/completions/home-manager.zsh
    source $HOME/.zsh/completions/gh.zsh
    # open () { xdg-open "$*" &}
    # alias o="thunar"
    ;;
  CYGWIN*|MINGW32*|MSYS*|MINGW*)
    ;;
  *)
    ;;
esac

export ZDOTDIR=$HOME/.zsh
source $HOME/.zsh_aliases
source $HOME/.zsh_exports
source $HOME/.zsh_functions
fpath+=$HOME/.zsh/completions
fpath=($HOME/.zsh/completions/nix-zsh-completions $fpath)

# ------------------------------- ZSH PLUGINS ---------------------------------
eval "$(starship init zsh)"
source $HOME/.zsh/plugins/git-flow-completion/git-flow-completion.zsh
source $HOME/.zsh/plugins/zsh-system-clipboard/zsh-system-clipboard.zsh
source $HOME/.zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source $HOME/.zsh/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh

. $HOME/.zsh/fzf-gems/fzf_git_functions.sh
. $HOME/.zsh/fzf-gems/fzf_git_keybindings.zsh


# ------------------------------- ZSH APPS ------------------------------------
eval "$(mcfly init zsh)"
export MCFLY_KEY_SCHEME=vim
