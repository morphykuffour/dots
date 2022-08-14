#!/usr/bin/env bash

echo "" | rofi -dmenu -p "Search:" | xargs -I{} xdg-open "https://search.nixos.org/packages?channel=22.05&from=0&size=50&sort=relevance&type=packages&query="{}
