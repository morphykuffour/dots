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

# terminal colors
Reset = "\x1b[0m"
Bright = "\x1b[1m"
Dim = "\x1b[2m"
Underscore = "\x1b[4m"
Blink = "\x1b[5m"
Reverse = "\x1b[7m"
Hidden = "\x1b[8m"

FgBlack = "\x1b[30m"
FgRed = "\x1b[31m"
FgGreen = "\x1b[32m"
FgYellow = "\x1b[33m"
FgBlue = "\x1b[34m"
FgMagenta = "\x1b[35m"
FgCyan = "\x1b[36m"
FgWhite = "\x1b[37m"

BgBlack = "\x1b[40m"
BgRed = "\x1b[41m"
BgGreen = "\x1b[42m"
BgYellow = "\x1b[43m"
BgBlue = "\x1b[44m"
BgMagenta = "\x1b[45m"
BgCyan = "\x1b[46m"
BgWhite = "\x1b[47m"

# mate with i3
`https://www.youtube.com/watch?v=IYAzUIFonEo`

- laptop sleeps if lid closed when external monitor is connected #318
```bash
gsettings get org.mate.power-manager button-lid-ac 'nothing'
```
