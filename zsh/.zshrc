#!/usr/bin/env zsh

# Set ZDOTDIR early so it's available for all config files
export ZDOTDIR=$HOME/.zsh

# kitty integration
if [[ -n $KITTY_INSTALLATION_DIR ]]; then
    export KITTY_SHELL_INTEGRATION="enabled"
    autoload -Uz -- "$KITTY_INSTALLATION_DIR"/shell-integration/zsh/kitty-integration
    kitty-integration
    unfunction kitty-integration
fi

# OpenCode session export aliases
alias opencode-export='~/.opencode/export-sessions.sh'
alias opencode-sessions='ls -la ~/.opencode/exports/sessions/'

# vterm integration for Emacs
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'

    vterm_printf() {
        if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ]); then
            # Tell tmux to pass the escape sequences through
            printf "\ePtmux;\e\e]%s\007\e\\" "$1"
        elif [ "${TERM%%-*}" = "screen" ]; then
            # GNU screen (screen, screen-256color, screen-256color-bce)
            printf "\eP\e]%s\007\e\\" "$1"
        else
            printf "\e]%s\e\\" "$1"
        fi
    }

    vterm_prompt_end() {
        vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
    }
    setopt PROMPT_SUBST
    PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
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
# Speed up compinit by skipping security check on cache if < 24h old
autoload -Uz compinit
setopt EXTENDEDGLOB
local zcompdump="${ZDOTDIR:-$HOME}/.cache/zsh/zcompdump"
# Disabled zwc compilation - minimal benefit on modern SSDs
# Only check cache once per day
if [[ -n ${zcompdump}(#qNmh-24) ]]; then
  compinit -C -d "$zcompdump"
else
  compinit -d "$zcompdump"
fi
unsetopt EXTENDEDGLOB

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
# enable terminal linewrap (skip if in kitty as it handles this)
[[ "$TERM" != "xterm-kitty" ]] && setterm -linewrap on 2> /dev/null

# If this is an xterm set the title to user@host:dir
case "$TERM" in
    xterm*|rxvt*|Eterm|aterm|kterm|gnome*|alacritty)
        precmd() { print -Pnr -- $'\e]0;%n@%m: %~\a' }
        ;;
esac

# Note: compinit already called above with optimizations
zmodload zsh/complist

#  ---------------------------- Key bindings -------------------------------------
# vi mode
bindkey -v
export KEYTIMEOUT=1

# Visual feedback for yank operations (like vim's highlight on yank)
# Load sched module for timing
zmodload zsh/sched 2>/dev/null

function _clear_yank_highlight_widget() {
    region_highlight=()
    zle -R 2>/dev/null
}
zle -N _clear_yank_highlight_widget

# Wrapper for vi-yank that adds highlight
function _vi-yank-with-highlight() {
    zle .vi-yank
    if [[ -n $CUTBUFFER ]]; then
        local start=$MARK
        local end=$CURSOR
        if (( start > end )); then
            local tmp=$start
            start=$end
            end=$tmp
        fi
        region_highlight=("$start $end bg=yellow fg=black")
        zle -R
        # Clear highlight after 0.2 seconds using sched
        sched +0.2 'zle && zle _clear_yank_highlight_widget' 2>/dev/null
    fi
}

# Wrapper for vi-yank-eol that adds highlight
function _vi-yank-eol-with-highlight() {
    local old_cursor=$CURSOR
    zle .vi-yank-eol
    if [[ -n $CUTBUFFER ]]; then
        region_highlight=("$old_cursor $CURSOR bg=yellow fg=black")
        zle -R
        sched +0.2 'zle && zle _clear_yank_highlight_widget' 2>/dev/null
    fi
}

# Wrapper for vi-yank-whole-line that adds highlight
function _vi-yank-whole-line-with-highlight() {
    zle .vi-yank-whole-line
    if [[ -n $CUTBUFFER ]]; then
        local bol=0
        local eol=${#BUFFER}
        for (( i=$CURSOR-1; i>=0; i-- )); do
            if [[ ${BUFFER[$i]} == $'\n' ]]; then
                bol=$((i+1))
                break
            fi
        done
        for (( i=$CURSOR+1; i<=${#BUFFER}; i++ )); do
            if [[ ${BUFFER[$i]} == $'\n' ]]; then
                eol=$i
                break
            fi
        done
        region_highlight=("$bol $eol bg=yellow fg=black")
        zle -R
        sched +0.2 'zle && zle _clear_yank_highlight_widget' 2>/dev/null
    fi
}

# Replace the default widgets with our wrappers
zle -N vi-yank _vi-yank-with-highlight
zle -N vi-yank-eol _vi-yank-eol-with-highlight
zle -N vi-yank-whole-line _vi-yank-whole-line-with-highlight


# Enhanced cursor shape for vim mode (tmux compatible)
function _cursor_mode() {
    local cursor_style="$1"
    # Check if we're in tmux and send proper escape sequences
    if [[ -n "$TMUX" ]]; then
        # Wrap cursor sequences for tmux passthrough
        printf '\ePtmux;\e\e[%s q\e\\' "$cursor_style"
    else
        # Direct escape sequence
        printf '\e[%s q' "$cursor_style"
    fi
}

function zle-keymap-select() {
    case $KEYMAP in
        vicmd)
            _cursor_mode 2  # Block cursor for normal mode
            ;;
        main|viins|*)
            _cursor_mode 6  # Beam cursor for insert mode
            ;;
    esac
}
zle -N zle-keymap-select

function zle-line-init() {
    zle -K viins  # Start in insert mode
    _cursor_mode 6  # Beam cursor
}
zle -N zle-line-init

# Reset to beam cursor before each command and on startup
_cursor_mode 6
preexec() { 
    _cursor_mode 6
}

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

# Alt+e: run command, edit output in $EDITOR, replace buffer
source ${${(%):-%x}:A:h}/.edit-command-output.zsh

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

# BROWSER is configured in .zsh_exports with better cross-platform detection

# Autojump - check Nix path first, then Homebrew
if [ -f /etc/profiles/per-user/$USER/etc/profile.d/autojump.sh ]; then
    . /etc/profiles/per-user/$USER/etc/profile.d/autojump.sh
elif [ -f /opt/homebrew/etc/profile.d/autojump.sh ]; then
    . /opt/homebrew/etc/profile.d/autojump.sh
elif [ -f $HOME/.zsh/completions/autojump.zsh ]; then
    source $HOME/.zsh/completions/autojump.zsh
fi

# OS-specific settings
case "$(uname -s)" in
  Darwin)
    code () { VSCODE_CWD="$PWD" open -n -b "com.microsoft.VSCode" --args $* ;}
    # o() function in .zsh_functions handles cross-platform open
    ;;
  Linux)
    # source $HOME/.zsh/completions/home-manager.zsh
    [ -f $HOME/.zsh/completions/gh.zsh ] && source $HOME/.zsh/completions/gh.zsh
    # open () { xdg-open "$*" &}
    # alias o="thunar"
    ;;
  CYGWIN*|MINGW32*|MSYS*|MINGW*)
    ;;
  *)
    ;;
esac

# Source zsh config files (zwc compilation disabled - minimal benefit on SSDs)
[ -f $HOME/.zsh_aliases ] && source $HOME/.zsh_aliases
[ -f $HOME/.zsh_exports ] && source $HOME/.zsh_exports
[ -f $HOME/.zsh_functions ] && source $HOME/.zsh_functions

# Add completions to fpath if they exist
[ -d $HOME/.zsh/completions ] && fpath+=$HOME/.zsh/completions
[ -d $HOME/.zsh/completions/nix-zsh-completions ] && fpath=($HOME/.zsh/completions/nix-zsh-completions $fpath)

# ------------------------------- ZSH PLUGINS ---------------------------------
# STARSHIP_CONFIG is set in .zsh_exports

# Cache directory for init scripts
_zsh_cache_dir="${XDG_CACHE_HOME:-$HOME/.cache}/zsh"
[[ -d "$_zsh_cache_dir" ]] || mkdir -p "$_zsh_cache_dir"

# Helper function to cache and source init scripts
# Usage: _cache_eval "command" "cache_name"
_cache_eval() {
    local cmd="$1"
    local cache_name="$2"
    local cache_file="$_zsh_cache_dir/${cache_name}.zsh"
    local bin_path

    # Get the binary path
    bin_path="$(command -v "${cmd%% *}" 2>/dev/null)" || return 1

    # Regenerate cache if binary is newer or cache doesn't exist
    if [[ ! -s "$cache_file" || "$bin_path" -nt "$cache_file" ]]; then
        eval "$cmd" > "$cache_file" 2>/dev/null
    fi

    source "$cache_file"
}

# Starship prompt (cached)
_cache_eval "starship init zsh" "starship"

# Load essential plugins immediately
[ -f $HOME/.zsh/plugins/git-flow-completion/git-flow-completion.zsh ] && source $HOME/.zsh/plugins/git-flow-completion/git-flow-completion.zsh

# Load zsh-defer for proper async plugin loading
[ -f $HOME/.zsh/plugins/zsh-defer/zsh-defer.plugin.zsh ] && source $HOME/.zsh/plugins/zsh-defer/zsh-defer.plugin.zsh

# Defer non-essential plugins to load asynchronously after prompt appears
# This significantly speeds up shell startup while keeping all plugins functional
if (( $+functions[zsh-defer] )); then
  # Defer loading to after prompt appears for fast startup
  zsh-defer source $HOME/.zsh/plugins/zsh-system-clipboard/zsh-system-clipboard.zsh
  zsh-defer source $HOME/.zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
  zsh-defer source $HOME/.zsh/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh
  zsh-defer source $HOME/.zsh/fzf-gems/fzf_git_functions.sh
  zsh-defer source $HOME/.zsh/fzf-gems/fzf_git_keybindings.zsh
else
  # Fallback: load immediately if zsh-defer is not available
  [ -f $HOME/.zsh/plugins/zsh-system-clipboard/zsh-system-clipboard.zsh ] && source $HOME/.zsh/plugins/zsh-system-clipboard/zsh-system-clipboard.zsh
  [ -f $HOME/.zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh ] && source $HOME/.zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
  [ -f $HOME/.zsh/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh ] && source $HOME/.zsh/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh
  [ -f $HOME/.zsh/fzf-gems/fzf_git_functions.sh ] && source $HOME/.zsh/fzf-gems/fzf_git_functions.sh
  [ -f $HOME/.zsh/fzf-gems/fzf_git_keybindings.zsh ] && source $HOME/.zsh/fzf-gems/fzf_git_keybindings.zsh
fi

# ------------------------------- ZSH APPS ------------------------------------
# MCFLY_KEY_SCHEME is set in .zsh_exports

# Mcfly (cached and deferred for faster startup)
if (( $+functions[zsh-defer] )); then
    zsh-defer _cache_eval "mcfly init zsh" "mcfly"
else
    _cache_eval "mcfly init zsh" "mcfly"
fi


# Source local environment variables (not tracked in dotfiles)
[ -f ~/.zshenv.local ] && source ~/.zshenv.local

# Added by LM Studio CLI (lms)
export PATH="$PATH:/Users/morph/.lmstudio/bin"
# End of LM Studio CLI section

# source /Users/morph/.stremio_aliases
# PATH additions are in .zsh_exports and .zprofile

# Tailscale SSH aliases - loaded from ~/.zshenv.local for privacy
# Use: tssh list - to see available hosts
# opencode aliases are in .zsh_aliases
