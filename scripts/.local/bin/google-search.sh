#!/bin/bash

# URL='https://www.google.com/search?q='
# QUERY=$(echo '' | dmenu -p "Search:" -fn "JetBrains-Mono-Regular-Nerd-Font-Complete-Mono" -b)
# if [ -n "$QUERY" ]; then
#   xdg-open "${URL}${QUERY}" 2> /dev/null
#   exec i3-msg [class="^Brave$"] focus
# fi


# echo "" | rofi -dmenu -p "Search:" | xargs -I{} xdg-open https://www.google.de/search?q={}

surfraw -browser=$BROWSER $(sr -elvi | awk -F'-' '{print $1}' | sed '/:/d' | awk '{$1=$1};1' | rofi -kb-row-select "Tab" -kb-row-tab "Control+space" -color-window "#000000, #000000, #000000" -color-normal "#000000, #b3e774, #000000, #b3e774, #000000" -color-active "#000000, #b3e774, #000000, #b3e774, #000000" -color-urgent "#000000, #b3e774, #000000, #b3e774, #000000" -dmenu -mesg ">>> Tab = Autocomplete" -i -p "websearch: ")
