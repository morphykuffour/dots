export ZDOTDIR=$HOME/.config/zsh
source $ZDOTDIR/zshrc
source $ZDOTDIR/.zshrc
source $HOME/.zshrc
zsh_add_file "zsh-exports"
zsh_add_file "zsh-vim-mode"
zsh_add_file "zsh-aliases"
zsh_add_file "zsh-prompt"
zsh_add_plugin "zsh-users/zsh-autosuggestions"
zsh_add_plugin "zsh-users/zsh-syntax-highlighting"

alias luamake=/home/morp/Programs/lua-language-server/3rd/luamake/luamake

# Generated for envman. Do not edit.
[ -s "$HOME/.config/envman/load.sh" ] && source "$HOME/.config/envman/load.sh"


export PATH="$HOME/.poetry/bin:$PATH"
