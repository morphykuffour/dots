#!/bin/sh 
# Arch and Ubuntu (WSL2) Setup
# by Morphy Kuffour
# License: GNU GPLv3

# Install vim-plug for neovim
sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'


# dircolors installation

mkdir /tmp/LS_COLORS && curl -L https://api.github.com/repos/trapd00r/LS_COLORS/tarball/master | tar xzf - --directory=/tmp/LS_COLORS --strip=1

( cd /tmp/LS_COLORS && sh install.sh )

# To enable the colors, add the following line to your shell's start-up script:
# For Bourne shell (e.g. ~/.bashrc or ~/.zshrc):
# . "~/.local/share/lscolors.sh"


# Make zsh the default shell
chsh -s /bin/zsh "$name" >/dev/null 2>&1
sudo -u "$name" mkdir -p "/home/$name/.cache/zsh/"

# Arch Setup
arch_linux=$(uname -r)
if [ $arch_linux = 'arch' ] ; then # arch-linux
    #install packages from ~/dotfiles/programs/arch_pkglist.txt
    pacman -S --needed $(comm -12 <(pacman -Slq | sort) <(sort ~/dotfiles/programs/arch_pkglist.txt))

# Start/restart PulseAudio.
killall pulseaudio; sudo -u "$name" pulseaudio --start

# This line, overwriting the `newperms` command above will allow the user to run
# serveral important commands, `shutdown`, `reboot`, updating, etc. without a password.
newperms "%wheel ALL=(ALL) ALL #LARBS
%wheel ALL=(ALL) NOPASSWD: /usr/bin/shutdown,/usr/bin/reboot,/usr/bin/systemctl suspend,/usr/bin/wifi-menu,/usr/bin/mount,/usr/bin/umount,/usr/bin/pacman -Syu,/usr/bin/pacman -Syyu,/usr/bin/packer -Syu,/usr/bin/packer -Syyu,/usr/bin/systemctl restart NetworkManager,/usr/bin/rc-service NetworkManager restart,/usr/bin/pacman -Syyu --noconfirm,/usr/bin/loadkeys,/usr/bin/paru,/usr/bin/pacman -Syyuw --noconfirm"

fi


# Ubuntu Setup
ubuntu_linux=$(lsb_release -a)
if [ $ubuntu_linux = 'Ubuntu' ] ; then # ubuntu
    #install packages from ~/dotfiles/programs/ubuntu_pkglist.txt
    sudo apt-get install build-enssential
    sudo apt-get install gdb
    sudo apt-get install stow
    sudo apt-get install dircolors
    sudo apt-get install tldr
    sudo apt-get install zsh
    sudo apt-get install xclip
    sudo apt-get install xsel
    sudo apt install x11-apps -y    
fi



# this should be done last
cd ~/dotfiles; 
stow gdb;
stow zsh;

stow tmux;
ln -s ~/.config/tmux/.tmux.conf ~/.tmux.conf
