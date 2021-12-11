#!/bin/sh
alias zsh-update-plugins="find "$ZDOTDIR/plugins" -type d -exec test -e '{}/.git' ';' -print0 | xargs -I {} -0 git -C {} pull -q"

# Easier navigation: .., ..., ~ and -
alias ls="exa"
alias cd..="cd .."
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias cd..="cd .."
alias cd...="cd ../.."
alias cd....="cd ../../.."
alias cd.....="cd ../../../.."
alias -- -="cd -"
alias ~="cd ~"
alias d="dirs -v"

# fuzzy search over your popularly visited directories
# alias g="new_loc=\$(cat ~/.local/share/autojump/autojump.txt | sort -n | grep -Po '^[^\s]+\s+(\K.*)' | fzf +s -e) && cd \"\$new_loc\""

# Colorize grep output (good for log files)
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

# confirm before overwriting something
alias cp="cp -i"
alias mv='mv -i'
alias rm='rm -i'

# easier to read disk
alias df='df -h'                          # human-readable sizes
alias free='free -m'                      # show sizes in MB

# get top process eating memory
alias psmem='ps auxf | sort -nr -k 4 | head -5'

# get top process eating cpu ##
alias pscpu='ps auxf | sort -nr -k 3 | head -5'

# gpg encryption
# verify signature for isos
alias gpg-check="gpg2 --keyserver-options auto-key-retrieve --verify"
# receive the key of a developer
alias gpg-retrieve="gpg2 --keyserver-options auto-key-retrieve --receive-keys"

# For when keys break
alias archlinx-fix-keys="sudo pacman-key --init && sudo pacman-key --populate archlinux && sudo pacman-key --refresh-keys"

# systemd
alias list_systemctl="systemctl list-unit-files --state=enabled"

# colorize stuff, utility commands
if ls --color >/dev/null 2>&1; then # GNU `ls`
    colorflag="--color=auto"
else # OS X `ls`
    colorflag="-G"
fi
if [[ "$INSIDE_EMACS" ]]; then
    colorflag=""
fi
#

# pretty ls
# List all files colorized in long format
# alias l="ls -Fl ${colorflag}"
alias l="exa --long --header --inode --git"
alias ltree="exa --long --header --inode --git"
alias ls-a="ls -Fa ${colorflag}"
# List all files colorized in long format, including dot files
alias la="ls -Fla ${colorflag}"
alias ll="ls -Fl ${colorflag}"
# List only directories
alias lsd='ls -Fl ${colorflag} | grep "^d"'
# Always use color output for `ls`
alias ls="command ls -F ${colorflag}"
#

# Set personal aliases
alias dotfiles="cd $HOME/dotfiles"
alias explorer='/mnt/c/Windows/explorer.exe'
alias vrc='nvim /home/morp/.config/nvim/init.vim'
alias rd="rmdir"
alias ip='ip -color=auto'
alias diff='diff --color=auto'
alias grep='grep --color=auto'
alias pip='pip3'
alias python='python3'
alias py="python3"
alias weather='curl wttr.in'
alias bat="\bat"
alias lsb="lsb_release -a"
alias nvdir="cd $HOME/.config/nvim"
# alias lua="lua5.2"
alias h="history"
alias v="nvim"
alias :e="$EDITOR ."
alias e="$EDITOR"
alias v.="$EDITOR ."
alias t='tmux'
alias r=ranger-cd
alias :q='exit'

# Edit config files
alias zshrc="$EDITOR $ZDOTDIR/zshrc"
alias vrc="$EDITOR  /home/morp/.config/nvim/init.vim"

alias vim='nvim'
alias sv="sudo $EDITOR"

# system management aliases
alias hibernate="systemctl suspend"
alias pacman="yay"

# utility commands
alias q="exit"
alias mypw="pwgen -c -n -s -y 16 -1"
alias ndate="date \"+%d-%m-%y\""

# easy reload of zsh stuff
alias rl="exec zsh -l"

# git configs
alias g="git"
alias gca="git commit --all --verbose"
alias gco="git checkout"
alias gs="git status --short"
alias gpush="git push -u origin HEAD"
alias gpull="git pull --rebase"
