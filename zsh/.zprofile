#!/bin/zsh

. "$HOME/.cargo/env"
HISTSIZE=1000000
SAVEHIST=1000000
export PATH="$HOME/.local/bin":$PATH
export PATH=$HOME/.cargo/bin:$PATH
export PATH=$HOME/.local/share/go/bin:$PATH
export GOPATH=$HOME/.local/share/go


if [[ -n "$IS_WSL" || -n "$WSL_DISTRO_NAME" ]]; then
    # WSL environment
    export BROWSER="/mnt/c/Program Files/BraveSoftware/Brave-Browser/Application/brave.exe"
    export DISPLAY="$(hostname).local:0.0"
else
    export BROWSER="brave-browser"
fi



export PATH=$PATH:./node_modules/.bin
export PATH="$HOME/.poetry/bin:$PATH"
export GUIX_LOCPATH="$HOME/.guix-profile/lib/locale"

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
    export PATH="/opt/homebrew/opt/coreutils/libexec/gnubin:$PATH"
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
