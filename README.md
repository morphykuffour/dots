# NixOS dotfiles.

Install `git` and GNU `stow`

Clone repo into your `$HOME` directory or `~`

```bash
cd $HOME
git clone https://github.com/morphykuffour/dotfiles.git 
```

Run `stow` to symlink everything or just select what you want

```bash
stow */ # Everything (the '/' ignores the README)
stow zsh # Just my zsh config
```
