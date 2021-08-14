# dotfiles  
Arch Linux installation with dwm as the windows manger

Ubuntu WSL for windows 

## Installing

Clone into your `$HOME` directory or `~`

```bash
git clone https://github.com/morphykuffour/dotfiles.git ~;

cd ~/dotfiles;

# Run this script to install necessary applications for Ubuntu
sh ~/dotfiles/install/ubuntu_install_tools.sh;

# Run this script to install necessary applications for Arch Linux
sh ~/dotfiles/install/arch_install_tools.sh;
```

Run `stow` to symlink everything or just select what you want

```bash
cd ~/dotfiles/xdg_config/
stow */ # Everything (the '/' ignores the README)
```

```bash
stow zsh # Just my zsh config
stow gdb # Just my gdb config
```

## Programs

An updated list of all the programs I use can be found in the `programs` directory
