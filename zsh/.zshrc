#!/usr/bin/env zsh

COLOR_SCHEME=dark # dark/light
# --------------------------------- ALIASES -----------------------------------
source $HOME/.zsh_aliases
source $HOME/.zsh_exports

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
HISTFILE=~/.cache/zsh/history
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

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
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

# Change cursor shape for different vi modes.
function zle-keymap-select () {
    case $KEYMAP in
        vicmd) echo -ne '\e[1 q';;      # block
        viins|main) echo -ne '\e[5 q';; # beam
    esac
}

zle -N zle-keymap-select
zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
    echo -ne "\e[5 q"
}

zle -N zle-line-init
echo -ne '\e[5 q' # Use beam shape cursor on startup.
preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.

# -------------------------------- FUNCTIONS ---------------------------------

# Use lf to switch directories and bind it to ctrl-o
lfcd () {
    tmp="$(mktemp)"
    lf -last-dir-path="$tmp" "$@"
    if [ -f "$tmp" ]; then
        dir="$(cat "$tmp")"
        rm -f "$tmp" >/dev/null
        [ -d "$dir" ] && [ "$dir" != "$(pwd)" ] && cd "$dir"
    fi
}
bindkey -s '^o' 'lfcd\n'

# bindkey -s '^a' 'bc -lq\n'

bindkey -s '^f' 'cd "$(dirname "$(fzf)")"\n'

bindkey '^[[P' delete-char

# Edit line in vim with ctrl-e:
autoload edit-command-line; zle -N edit-command-line
bindkey '^e' edit-command-line

zstyle ':fzf-tab:complete:*:*' fzf-preview 'less ${(Q)realpath}'
export LESSOPEN='| ~/.local/bin/lessfilter %s'
# disable sort when completing `git checkout`
zstyle ':completion:*:git-checkout:*' sort false
# set descriptions format to enable group support
zstyle ':completion:*:descriptions' format '[%d]'
# set list-colors to enable filename colorizing
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
# preview directory's content with exa when completing cd
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'exa -1 --color=always $realpath'
# switch group using `,` and `.`
zstyle ':fzf-tab:*' switch-group ',' '.'

# ------------------------------- ZSH FUNCTIONS ---------------------------------
change_background() {
    # dconf write /org/mate/desktop/background/picture-filename "'$HOME/Pictures/wallpapers/$(ls $HOME/Pictures/wallpapers | fzf)'"
    feh --bg-scale $HOME/Pictures/wallpapers/$(ls $HOME/Pictures/wallpapers | fzf --preview 'sxiv {}')
}

die () {
    echo >&2 "$@"
    exit 1
}

addToPath() {
    if [[ "$PATH" != *"$1"* ]]; then
        export PATH=$PATH:$1
    fi
}

addToPathFront() {
    if [[ "$PATH" != *"$1"* ]]; then
        export PATH=$1:$PATH
    fi
}

commitDotFiles() {
    cd $HOME/dotfiles
    git add .
    git commit -am "dotfile update"
    git push 
}

mkd() {
  if [ ! -n "$1" ]; then
    echo "Enter a directory name"
  elif [ -d $1 ]; then
    echo "\`$1' already exists"
  else
    mkdir $1 && cd $1
  fi
}

build_cscope_db_func() {
    local PROJDIR=$PWD
    cd /
    find $PROJDIR -name \\''*.c\\'' -o -name \\''*.h\\'' > $PROJDIR/cscope.files
    cd $PROJDIR
    cscope -Rbkq
}
alias csbuild=\\''build_cscope_db_func\\''
# https://medium.com/@mmeinhar85/how-to-integrate-cscope-into-vim-in-linux-85274102474
# fix tmux issue
# Change cursor with support for inside/outside tmux
# function _set_cursor() {
#     if [[ $TMUX = '' ]]; then
#       echo -ne $1
#     else
#       echo -ne "\ePtmux;\e\e$1\e\\"
#     fi
# }
# 
# function _set_block_cursor() { _set_cursor '\e[2 q' }
# function _set_beam_cursor() { _set_cursor '\e[6 q' }
# 
# function zle-keymap-select {
#   if [[ ${KEYMAP} == vicmd ]] || [[ $1 = 'block' ]]; then
#       _set_block_cursor
#   else
#       _set_beam_cursor
#   fi
# }
# zle -N zle-keymap-select

# ensure beam cursor when starting new terminal
# precmd_functions+=(_set_beam_cursor) #
# ensure insert mode and beam cursor when exiting vim
# zle-line-init() { zle -K viins; _set_beam_cursor }


# ------------------------------- ZSH APPS ------------------------------------
eval "$(atuin init zsh)"
eval "$(mcfly init zsh)"
eval "$(starship init zsh)"

# case "$(uname -s)" in
#    Darwin)
# #    eval $(/opt/homebrew/bin/brew shellenv)
#      ;;
#    Linux)
#      echo 'Linux'
# #     eval "$(starship init zsh)"
#      ;;
#    CYGWIN*|MINGW32*|MSYS*|MINGW*)
#      ;;
#    *)
#      # echo 'Other OS' 
#      ;;
# esac

# I love this Prompt
# PROMPT=$'%F{%(#.blue.green)}┌──(%B%F{%(#.red.blue)}%n@%m%b%F{%(#.blue.green)})-[%B%F{reset}%(6~.%-1~/…/%4~.%5~)%b%F{%(#.blue.green)}]\n└─%B%(#.%F{red}#.%F{blue}$)%b%F{reset} '
# RPROMPT=$'%(?.. %? %F{red}%Bx%b%F{reset})%(1j. %j %F{yellow}%Bbg %b%F{reset}.)'

# ------------------------------- ZSH PLUGINS ---------------------------------
# source /home/morp/.zsh/fzf-tab/fzf-tab.plugin.zsh
fpath=($HOME/.zsh/zsh-completions/src $fpath)
source $HOME/.zsh/git-flow-completion/git-flow-completion.zsh
source $HOME/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
source $HOME/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
