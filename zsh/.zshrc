#!/usr/bin/env zsh
# kitty integration
if [[ -n $KITTY_INSTALLATION_DIR ]]; then
    export KITTY_SHELL_INTEGRATION="enabled"
    autoload -Uz -- "$KITTY_INSTALLATION_DIR"/shell-integration/zsh/kitty-integration
    kitty-integration
    unfunction kitty-integration
fi

export ZDOTDIR=$HOME/.zsh
source $HOME/.zsh_aliases
source $HOME/.zsh_exports
source $HOME/.zsh_functions
fpath+=$HOME/.zsh/completions
fpath=($HOME/.zsh/completions/nix-zsh-completions $fpath)


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
chpwd() {
    exa
}

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
    [[ -s $(brew --prefix)/etc/profile.d/autojump.sh ]] && . $(brew --prefix)/etc/profile.d/autojump.sh
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

# ------------------------------- ZSH FUNCTIONS -------------------------------

# Function to source files if they exist
zsh_add_file() {
    [ -f "$ZDOTDIR/$1" ] && source "$ZDOTDIR/$1"
}

# source:https://stackoverflow.com/a/65375231/2571881
# ~/.dots/zsh/autoload/vif
function vif() {
    local fname
    local current_dir=$PWD
    cd ~/dots || exit
    fname=$(fzf --preview 'bat {} ') || return
    nvim "$fname"
    cd $current_dir
}
# https://jdhao.github.io/2019/06/13/zsh_bind_keys/
bindkey -s '^p' 'vif^M'

function vao() {
    fname=$(fzf --preview 'bat {} ') || return
    nvim "$fname"
}
bindkey -s '^f' 'vao^M'

# source:https://stackoverflow.com/a/65375231/2571881
# ~/.dots/zsh/autoload/vif
function nif() {
    local fname
    local current_dir=$PWD
    cd ~/nix || exit
    fname=$(fzf --preview 'bat {} ') || return
    nvim "$fname"
    cd $current_dir
}
# https://jdhao.github.io/2019/06/13/zsh_bind_keys/
bindkey -s '^n' 'nif^M'

# neovim-docker
dv () {
	sudo docker run \
	-it -v `pwd`:/mnt/volume\
        -v $HOME/.config/nvim:/home/neovim/.config/nvim \
	--workdir=/mnt/volume \
	anatolelucet/neovim:stable \
        "$@"
}

# Bash Function To Extract File Archives Of Various Types
extract () {
     if [ -f $1 ] ; then
         case $1 in
             *.tar.bz2)   tar xjf $1     ;;
             *.tar.gz)    tar xzf $1     ;;
             *.tar.xz)    tar xf  $1     ;;
             *.bz2)       bunzip2 $1     ;;
             *.rar)       rar x   $1     ;;
             *.gz)        gunzip  $1     ;;
             *.tar)       tar xf  $1     ;;
             *.tbz2)      tar xjf $1     ;;
             *.tgz)       tar xzf $1     ;;
             *.zip)       unzip   $1     ;;
             *.Z)         uncompress $1  ;;
             *.7z)        7z x    $1     ;;
             *.zst)       unzstd  $1     ;;
             *.xz)        unxz    $1     ;;
             *)           echo "'$1' cannot be extracted via extract()" ;;
         esac
     else
         echo "'$1' is not a valid file"
     fi
}

zsh_add_plugin() {
  PLUGIN_NAME=$(basename "$1" | cut -f 1 -d '.')
    if [ -d "$ZDOTDIR/plugins/$PLUGIN_NAME" ]; then 
        # For plugins
        zsh_add_file "plugins/$PLUGIN_NAME/$PLUGIN_NAME.plugin.zsh" || \
        zsh_add_file "plugins/$PLUGIN_NAME/$PLUGIN_NAME.zsh"
    else
        git clone $1 "$ZDOTDIR/plugins/$PLUGIN_NAME"
    fi
}

# ranger change dir
# ranger-cd() {
#     tmp="$(mktemp)"
#     ranger --choosedir="$tmp" "$@"
#     if [ -f "$tmp" ]; then
#         dir="$(cat "$tmp")"
#         rm -f "$tmp"
#         if [ -d "$dir" ]; then
#             if [ "$dir" != "$(pwd)" ]; then
#                 cd "$dir"
#             fi
#         fi
#     fi
# }
# alias rcd=ranger-cd
# USE LF TO SWITCH DIRECTORIES AND BIND IT TO CTRL-O
lfcd () {
    tmp="$(mktemp)"
    lf -last-dir-path="$tmp" "$@"
    if [ -f "$tmp" ]; then
        dir="$(cat "$tmp")"
        rm -f "$tmp"
        [ -d "$dir" ] && [ "$dir" != "$(pwd)" ] && cd "$dir"
    fi
}
bindkey -s '^o' 'lfcd\n'
# bindkey -s '^O' "lf\n"
# bindkey -s '^o' 'nvim $(fzf)^M'
# bindkey -s '^o' "rcd\n"

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

# create a new script, automatically populating the shebang line, editing the script, and making it executable.
shebang() {
# $ shebang perl test.pl
    if i=$(which $1);
    then
        printf '#!/usr/bin/env %s\n\n' $1 > $2 && chmod 755 $2 && vim + $2 && chmod 755 $2;
    else
        echo "'which' could not find $1, is it in your \$PATH?";
    fi;
    rehash
}

commitDotFiles() {
    cd "$HOME"/dots || exit
    git add .
    git commit 
    # git push 
}

mkd() {
  if [ -z "$1" ]; then
    echo "Enter a directory name"
  elif [ -d "$1" ]; then
    echo "\`$1' already exists"
  else
    mkdir "$1" && cd "$1" || exit
  fi
}

# change_background() {
#     ls $HOME/Dropbox/Pictures/wallpapers | fzf --preview="feh --bg-scale $HOME/Dropbox/Pictures/wallpapers/{}" | xargs -I {} feh --bg-scale $HOME/Dropbox/Pictures/wallpapers/{}
# }

change_background() {
    feh --randomize --bg-scale --no-xinerama $HOME/Pictures/wallpapers/$(ls $HOME/Pictures/wallpapers | fzf --preview 'sxiv {}')
}

#Enhanced rm
function fzf-rm() {
  if [[ "$#" -eq 0 ]]; then
    local files
    files=$(find . -maxdepth 1 -type f | fzf --multi)
    echo $files | xargs -I '{}' rm {} #we use xargs so that filenames to capture filenames with spaces in them properly
  else
    command rm "$@"
  fi
}

#Eval commands on the fly
fzf-eval() {
   echo | fzf -q "$*" --preview-window=up:99% --preview="eval {q}"
}

## Search list of your aliases and functions
fzf-aliases-functions() {
    CMD=$(
        (
            (alias)
            (functions | grep "()" | cut -d ' ' -f1 | grep -v "^_" )
        ) | fzf | cut -d '=' -f1
    );

    eval $CMD
}

## File Finder (Open in $EDITOR)
fzf-find-files() {
  local file=$(fzf --multi --reverse) #get file from fzf
  if [[ $file ]]; then
    for prog in $(echo $file); #open all the selected files
    do; $EDITOR $prog; done;
  else
    echo "cancelled fzf"
  fi
}


# Find Dirs
fzf-cd() {
  local dir
  dir=$(find ${1:-.} -path '*/\.*' -prune \
                  -o -type d -print 2> /dev/null | fzf +m) &&
  cd "$dir"
  ls
}

# Find Dirs + Hidden
function fzf-cd-incl-hidden() {
  local dir
  dir=$(find ${1:-.} -type d 2> /dev/null | fzf +m) && cd "$dir"
  ls
}

# cd into the directory of the selected file
function fzf-cd-to-file() {
   local file
   local dir
   file=$(fzf +m -q "$1") && dir=$(dirname "$file") && cd "$dir"
   ls
}

# fdr - cd to selected parent directory
function fzf-cd-to-parent() {
  local declare dirs=()
  get_parent_dirs() {
    if [[ -d "${1}" ]]; then dirs+=("$1"); else return; fi
    if [[ "${1}" == '/' ]]; then
      for _dir in "${dirs[@]}"; do echo $_dir; done
    else
      get_parent_dirs $(dirname "$1")
    fi
  }
  local DIR=$(get_parent_dirs $(realpath "${1:-$PWD}") | fzf-tmux --tac)
  cd "$DIR"
  ls
}

# Search env variables
function fzf-env-vars() {
  local out
  out=$(env | fzf)
  echo $(echo $out | cut -d= -f2)
}


# Kill process
function fzf-kill-processes() {
  local pid
  pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')

  if [ "x$pid" != "x" ]
  then
    echo $pid | xargs kill -${1:-9}
  fi
}
# from: http://www.jaakkoluttinen.fi/blog/conda-on-nixos/
function conda-shell {
    nix-shell ~/.conda-shell.nix
}


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
# export ATUIN_NOBIND="true"
# eval "$(atuin init zsh)"
# bindkey '^r' _atuin_search_widget
# bindkey '^[[A' _atuin_search_widget
# bindkey '^[OA' _atuin_search_widget

# opam configuration
[[ ! -r /Users/user/.opam/opam-init/init.zsh ]] || source /Users/user/.opam/opam-init/init.zsh  > /dev/null 2> /dev/null
