#! /bin/bash

dots=$HOME/dotfiles
config=$HOME/.config

set -x

if [[ -d "$HOME/.config" ]]; then
    MOVE_CONFIG=true
fi

if [[ $MOVE_CONFIG ]]; then
    echo "Moving config"
    mv ~/.config ~/_temp_config
fi

ln -sv ~/dotfiles/xdg_config/ ~/.config

if [[ $MOVE_CONFIG ]]; then
    cp -r ~/_temp_config/* ~/.config/
    rm -rf ~/_temp_config
fi

# zshenv
if [[ -f "~/.zshenv" ]]; then
    ln -vs ~/dotfiles/xdg_config/zsh/.config/zsh/.zshenv ~/.zshenv
fi

ln -fs $dots/xdg_config/tmux/.config/tmux $config/tmux/

# Font setup
mkdir -p $HOME/.local/share/fonts
cp $dots/font/JetBrains_Mono_Regular_Nerd_Font_Complete_Mono.ttf ~/.local/share/fonts/
sudo fc-cache -fv

echo
echo "Finished deployment"
