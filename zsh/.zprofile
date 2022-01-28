#!/bin/zsh

. "$HOME/.cargo/env"
HISTSIZE=1000000
SAVEHIST=1000000
export PATH="$HOME/.local/bin":$PATH
export PATH=$HOME/.cargo/bin:$PATH
export PATH=$HOME/.local/share/go/bin:$PATH
export GOPATH=$HOME/.local/share/go

export VISUAL=nvim
export EDITOR=nvim
export FILE=lf
export TERMINAL=kitty
export BROWSER="brave-browser"
export LESS_TERMCAP_mb=$'\e[1;32m'
export LESS_TERMCAP_md=$'\e[1;32m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[01;33m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[1;4;31m'


export PATH=$PATH:./node_modules/.bin
export PATH="$HOME/.poetry/bin:$PATH"
# start emacs 

case "$(uname -s)" in
   Darwin)
#    eval $(/opt/homebrew/bin/brew shellenv)
      /opt/homebrew/bin/emacs --daemon &
      [[ -s $(brew --prefix)/etc/profile.d/autojump.sh ]] && . $(brew --prefix)/etc/profile.d/autojump.sh
     ;;
   Linux)
     echo 'Linux'
#     eval "$(starship init zsh)"
      /usr/bin/emacs --daemon &
     ;;
   CYGWIN*|MINGW32*|MSYS*|MINGW*)
     ;;
   *)
     # echo 'Other OS' 
     ;;
esac
