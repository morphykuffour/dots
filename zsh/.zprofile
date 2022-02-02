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

case "$(uname -s)" in
  Darwin)
    #    eval $(/opt/homebrew/bin/brew shellenv)
    # start emacs 
    # /opt/homebrew/bin/emacs --daemon &

    # Setting PATH for Python 3.9
    # The original version is saved in .zprofile.pysave
    PATH="/Library/Frameworks/Python.framework/Versions/3.9/bin:${PATH}"
    PATH="/Library/Developer/CommandLineTools/usr/bin:${PATH}"
    export PATH
    export SHELL="/bin/zsh"
    alias o="open"
    # ~/.dircolors/themefile
    # eval $(dircolors ~/.dircolors/dircolors.256dark)

    # Aliases
    alias ls='gls --color=auto'
    alias ll='ls -al'

    ##
    # Your previous /Users/morphykuffour/.zprofile file was backed up as /Users/morphykuffour/.zprofile.macports-saved_2022-01-20_at_11:53:51
    ##

    # MacPorts Installer addition on 2022-01-20_at_11:53:51: adding an appropriate PATH variable for use with MacPorts.
    export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
    # Finished adapting your PATH environment variable for use with MacPorts.


    # Setting PATH for Python 3.10
    # The original version is saved in .zprofile.pysave
    PATH="/Library/Frameworks/Python.framework/Versions/3.10/bin:${PATH}"
    export PATH

    ;;
  Linux)
    export FILE=lf
    # echo 'Linux'
    #     eval "$(starship init zsh)"
    # /usr/bin/emacs --daemon &
    ;;
  CYGWIN*|MINGW32*|MSYS*|MINGW*)
    ;;
  *)
    # echo 'Other OS' 
    ;;
esac
