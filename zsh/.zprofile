#!/bin/zsh

# . "$HOME/.cargo/env"
HISTSIZE=1000000
SAVEHIST=1000000
export PATH="$HOME/.local/bin":$PATH
export PATH=$HOME/.cargo/bin:$PATH
export PATH=$HOME/.local/share/go/bin:$PATH
export GOPATH=$HOME/.local/share/go
export PATH=$PATH:./node_modules/.bin
export PATH="$HOME/.poetry/bin:$PATH"
export GUIX_LOCPATH="$HOME/.guix-profile/lib/locale"

