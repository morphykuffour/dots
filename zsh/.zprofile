#!/bin/zsh

# History settings
HISTSIZE=1000000
SAVEHIST=1000000

# PATH additions (only if not already present)
[[ "$PATH" != *"$HOME/.local/bin"* ]] && export PATH="$HOME/.local/bin:$PATH"
[[ "$PATH" != *"$HOME/.cargo/bin"* ]] && export PATH="$HOME/.cargo/bin:$PATH"
[[ "$PATH" != *"$HOME/.poetry/bin"* ]] && export PATH="$HOME/.poetry/bin:$PATH"

# Node modules (local)
export PATH=$PATH:./node_modules/.bin

# Guix locale
export GUIX_LOCPATH="$HOME/.guix-profile/lib/locale"

