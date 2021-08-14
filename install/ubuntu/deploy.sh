#! /bin/bash

dots=$HOME/dotfiles
config=$HOME/.config

set -x

if [[ -d "$HOME/.config" ]]; then
    MOVE_CONFIG=true
fi

if [[ $MOVE_CONFIG ]]; then
    echo "Moving config"
    mv $HOME/.config $HOME/.config.old
fi

ln -sv $HOME/dotfiles/config/ $HOME/.config

# zshenv
if [[ -f "$HOME/.zshenv" ]]; then
    ln -vs $HOME/dotfiles/xdg_config/zsh/.config/zsh/.zshenv $HOME/.zshenv
fi

ln  $HOME/.config/tmux/.tmux.conf $HOME/.tmux.conf/
ln  $dots/git/gitconfig $HOME/.gitconfig

# Font setup
mkdir -p $HOME/.local/share/fonts
cp $dots/font/JetBrains_Mono_Regular_Nerd_Font_Complete_Mono.ttf $HOME/.local/share/fonts/
sudo fc-cache -fv

echo
echo "Finished deployment"
