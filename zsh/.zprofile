#!/bin/zsh

# History settings - must match .zshrc
HISTSIZE=10000000
SAVEHIST=10000000

# PATH additions are centralized in .zsh_exports
# Only add poetry here as it's not in .zsh_exports
[[ "$PATH" != *"$HOME/.poetry/bin"* ]] && export PATH="$HOME/.poetry/bin:$PATH"

# Node modules (local project binaries)
export PATH=$PATH:./node_modules/.bin

# Guix locale (if using Guix)
export GUIX_LOCPATH="$HOME/.guix-profile/lib/locale"

