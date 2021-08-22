# dotfiles  
This main repo is for my arch linux dotfiles.

## Installing

You will need `git` and GNU `stow`
Clone into your `$HOME` directory or `~`

```bash
cd $HOME
git clone --single-branch --branch main https://github.com/morphykuffour/dotfiles.git 
```

Run `stow` to symlink everything or just select what you want

```bash
stow */ # Everything (the '/' ignores the README)
stow zsh # Just my zsh config
```

Run this script to install necessary applications for Arch Linux

```bash
bash ~/dotfiles/install/install_tools.sh;
bash ~/dotfiles/install/deploy.sh;

```
## Programs

An updated list of all the programs I use can be found in the `programs` directory
