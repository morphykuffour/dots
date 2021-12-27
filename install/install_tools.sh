#!/bin/sh 

# Make zsh the default shell
chsh -s /bin/zsh "$name" >/dev/null 2>&1
sudo -u "$name" mkdir -p "/home/$name/.cache/zsh/"

# Start/restart PulseAudio.
killall pulseaudio; sudo -u "$name" pulseaudio --start

