#!/bin/zsh

. "$HOME/.cargo/env"
HISTSIZE=1000000
SAVEHIST=1000000
export PATH="$HOME/.local/bin":$PATH
export PATH=$HOME/.cargo/bin:$PATH
export PATH=$HOME/.local/share/go/bin:$PATH
export GOPATH=$HOME/.local/share/go
export PATH=$PATH:./node_modules/.bin
export PATH="$HOME/.poetry/bin:$PATH"
export GUIX_LOCPATH="$HOME/.guix-profile/lib/locale"

# on MacOS X
# export ANDROID_HOME=/Users/$USER/Library/Android/sdk

# React Native
export ANDROID_HOME=$HOME/Android/Sdk
export ANDROID_SDK_ROOT=$HOME/Android/Sdk
export PATH=$PATH:$ANDROID_HOME/tools
export PATH=$PATH:$ANDROID_HOME/platform-tools
export PATH=$PATH:$ANDROID_HOME/emulator
export PATH=$PATH:$ANDROID_HOME/tools/bin
export PATH=$PATH:$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools
export NODE_OPTIONS=–openssl-legacy-provider
# export PATH=${PATH}:$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools
