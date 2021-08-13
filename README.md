# dotfiles  
Arch Linux installation with dwm as the windows manger

Ubuntu WSL for windows 

## Installing

Clone into your `$HOME` directory or `~`

```bash
git clone https://github.com/morphykuffour/dotfiles.git ~;

cd ~/dotfiles;

# Run this script to install necessary applications
sh ~/dotfiles/install.sh;
```

Run `stow` to symlink everything or just select what you want

```bash
stow */ # Everything (the '/' ignores the README)
```

```bash
stow zsh # Just my zsh config
stow gdb # Just my gdb config
```

## Programs

An updated list of all the programs I use can be found in the `programs` directory
