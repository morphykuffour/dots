#!/bin/bash 

mkdir -p ~/build

sudo -u "$USER" mkdir -p "/home/$USER/.cache/zsh/"

# build and install neovim
sudo apt update

sudo apt-get install -y \
  make cmake git \
  gettext libtool libtool-bin autoconf automake cmake g++ pkg-config unzip \
  make build-essential libssl-dev zlib1g-dev libbz2-dev \
  libreadline-dev libsqlite3-dev wget curl llvm libncurses5-dev libncursesw5-dev \
  xz-utils tk-dev libffi-dev liblzma-dev git

if ! [ -d "$HOME"/build/neovim ]; then
  git clone https://github.com/neovim/neovim ~/build/neovim
  cd ~/build/neovim/ || exit
  make
  sudo make install
fi

# {{{ Rust tools
# install cargo
if ! [ -x "$(command -v cargo)" ]; then
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
fi

if ! command -v rust_analyzer &> /dev/null ; then
  git clone https://github.com/rust-analyzer/rust-analyzer ~/build/rust-analyzer
  cd ~/build/rust-analyzer || exit
  cargo xtask install --server
fi

cargo install \
  git-trim \
  ripgrep \
  broot \
  starship

# install diff tool
if ! [ -d ~/build/delta ]; then
  git clone https://github.com/dandavison/delta ~/build/delta

  cd ~/build/delta || exit
  cargo install --path .
fi

# install Atuin History 
if ! [ -d ~/build/atuin ]; then
  git clone https://github.com/ellie/atuin.git
  cd atuin || exit
  cargo install --path .
fi

# install McFly CTRL + R
curl -LSfs https://raw.githubusercontent.com/cantino/mcfly/master/ci/install.sh | sudo sh -s -- --git cantino/mcfly
libx11-dev
libxinerama-dev
libxft-dev
libx11-xcb-dev
libxcb-res0-dev
# }}} 

# install Zsh
sudo apt install zsh

# install Github cli
sudo apt install gh

if ! command -v kitty &> /dev/null ; then
  curl -L https://sw.kovidgoyal.net/kitty/installer.sh | sh /dev/stdin
fi

# install Brave Browser
if ! command -v brave-browser &> /dev/null ; then
  sudo apt install apt-transport-https curl
  sudo curl -fsSLo /usr/share/keyrings/brave-browser-archive-keyring.gpg https://brave-browser-apt-release.s3.brave.com/brave-browser-archive-keyring.gpg
  echo "deb [signed-by=/usr/share/keyrings/brave-browser-archive-keyring.gpg arch=amd64] https://brave-browser-apt-release.s3.brave.com/ stable main"|sudo tee /etc/apt/sources.list.d/brave-browser-release.list
  sudo apt update
  sudo apt install brave-browser
fi

# install discord and discord-PTB
if ! command -v discord &> /dev/null ; then
  wget -O ~/Downloads/discord.deb "https://discordapp.com/api/download?platform=linux&format=deb"
  cd "$HOME"/Downloads || exit
  sudo apt install ./discord.deb
fi

if ! command -v discord-ptb &> /dev/null ; then 
  cd "$HOME"/Downloads || exit
  wget -O ~/Downloads/discord-ptb.deb "https://discordapp.com/api/download/ptb?platform=linux&format=deb"
  sudo apt install ./discord-ptb.deb
fi

# install dwm on debian
sudo apt install libx11-dev libxinerama-dev libxft-dev libx11-xcb-dev libxcb-res0-dev -y
# build patched-libxft-bgra
cd ~/build || exit
git clone https://github.com/uditkarode/libxft-bgra
cd libxft-bgra || exit
sh autogen.sh --sysconfdir=/etc --prefix=/usr --mandir=/usr/share/man
sudo make install
