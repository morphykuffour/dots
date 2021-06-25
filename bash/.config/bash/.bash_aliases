# Easier navigation: .., ..., ~ and -
alias ..="cd .."
alias cd..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias -- -="cd -"

# ls
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# mv, rm, cp
alias mv='mv -v'
alias rm='rm -i -v'
alias cp='cp -v'

# vim
alias nvimrc='nvimrc /home/morp/.config/nvim/init.vim'
alias v='nvim'
alias sv="sudo nvim"
alias vim='nvim'

# wsl
alias explorer='/mnt/c/Windows/explorer.exe'

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# Networking. IP address, dig, DNS
alias ip="dig +short myip.opendns.com @resolver1.opendns.com"
# alias wget="curl -O"

# Set personal aliases
alias zshrc="nvim ~/.zshrc"
alias bashrc="nvim ~/.bashrc"
alias ohmyzsh="nvim ~/.oh-my-zsh"
alias nvimrc='nvim /home/morp/.config/nvim/init.vim'
alias vrc='nvim /home/morp/.config/nvim/init.vim'
alias rd="rmdir"
alias update="sudo apt update && sudo apt upgrade -y"
alias ip='ip -color=auto'
alias diff='diff --color=auto'
alias grep='grep --color=auto'
alias pip='pip3'
alias python='python3'
alias weather='curl wttr.in'
alias lcthw="cd ~/Programs/CPrograms/LCTHW/"
alias bat="batcat"
alias pdfviewer="/mnt/c/Users/NAmoa/AppData/Local/SumatraPDF/SumatraPDF.exe"
alias brave="/mnt/c/Program Files/BraveSoftware/Brave-Browser/Application/brave.exe"
alias luamake=/home/morp/lua-language-server/3rd/luamake/luamake
alias SysInfo="lsb_release -a"
alias nvdir="cd $HOME/.config/nvim"
alias pdf="/mnt/c/Progam Files (x86)/Adobe/Acrobat Reader DC/Readerr/AcroRd32.exe"
alias ll='ls -halF'
alias ls='ls --classify --tabsize=0 --group-directories-first --literal --color=auto --show-control-chars --human-readable'
