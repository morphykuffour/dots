#!/usr/bin/env bash

URL='https://www.google.com/search?q='
QUERY=$(echo '' | dmenu -p "Search:" -fn "JetBrains-Mono-Regular-Nerd-Font-Complete-Mono" -b)
if [ -n "$QUERY" ]; then
  xdg-open "${URL}${QUERY}" 2> /dev/null
  exec i3-msg [class="^Brave$"] focus
fi

