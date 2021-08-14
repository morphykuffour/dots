#!/bin/sh 

set -e
set -x

mkdir -p ~/build
mkdir -p ~/git

if ! command -v gh &> /dev/null ; then
    sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-key C99B11DEB97541F0
    sudo apt-add-repository https://cli.github.com/packages
fi

sudo apt update -y

# {{{ Tools
sudo apt-get install -y \
    make cmake git \
    gettext libtool libtool-bin autoconf automake cmake g++ pkg-config unzip \
    make build-essential libssl-dev zlib1g-dev libbz2-dev \
    libreadline-dev libsqlite3-dev wget curl llvm libncurses5-dev libncursesw5-dev \
    xz-utils tk-dev libffi-dev manpages-dev liblzma-dev git

# {{{ Neovim
if ! [ -d $HOME/build/neovim ]; then
    git clone https://github.com/neovim/neovim ~/build/neovim
    cd ~/build/neovim/
    make
    sudo make install
fi

# {{{ Python tools
if ! [ -d $HOME/.pyenv ]; then
  curl https://pyenv.run | bash
fi

# {{{ Rust tools
if ! [ -x "$(command -v cargo)" ]; then
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
fi

if ! command -v rust_analyzer &> /dev/null ; then
    git clone https://github.com/rust-analyzer/rust-analyzer ~/build/rust-analyzer
    cd ~/build/rust-analyzer
    cargo xtask install --server
fi

cargo install \
  git-trim \
  ripgrep \
  broot \
  starship

if ! [ -d ~/build/delta ]; then
  git clone https://github.com/dandavison/delta ~/build/delta

  cd ~/build/delta
  cargo install --path .
fi


# {{{ Zsh
sudo apt install zsh -y

# Make zsh the default shell
chsh -s /bin/zsh "$name" >/dev/null 2>&1
sudo -u "$name" mkdir -p "/home/$name/.cache/zsh/"

# {{{ Github
sudo apt install gh -y

# dircolors installation

mkdir /tmp/LS_COLORS && curl -L https://api.github.com/repos/trapd00r/LS_COLORS/tarball/master | tar xzf - --directory=/tmp/LS_COLORS --strip=1

( cd /tmp/LS_COLORS && sh install.sh )

# {{{ Install Applications
sudo apt-get install -y \
     build-enssential gdb stow dircolors tldr zsh xclip xsel x11-apps -y    python3-pip

#install packages from ~/dotfiles/programs/ubuntu_pkglist.txt
