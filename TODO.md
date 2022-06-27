- [ ] replace sxiv with kitty icat
- [ ] QMK keyboards key mods
- [x] PageUp -> KC_WWW_SEARCH
- [x] MT(MOD_LCTL, KC_ESC)
- [x] LT(1, KC_TAB)
- [x] MT(MOD_LCTL | MOD_LALT, KC_CTL)
- [ ] move everything to Nix OS
  - [ ]  Support for WSL, Linux, Mac OS X


- neovim firecracker vm

- Containers
runc
docker
docker-compose

- Virtual Machines
firecracker
ignite
footloose


# Force-remove all running VMs
ignite rm -f $(ignite ps -aq)
