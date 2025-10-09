#!/usr/bin/env bash
set -euo pipefail

LANGFILE="${HOME}/.tmux-cht-languages"
CMDFILE="${HOME}/.tmux-cht-command"

# In case one of the files doesn't exist yet
touch "$LANGFILE" "$CMDFILE"

# Pick a topic/command
selected="$(cat "$LANGFILE" "$CMDFILE" | fzf --prompt='cht.sh > ' --height=40% --border)"
[ -z "${selected}" ] && exit 0

# Read query
read -r -p "Enter query: " query

# URL-encode helper (POSIX-ish)
urlencode() {
  # shellcheck disable=SC2018,SC2019
  local LC_ALL=C
  local s="$*"
  local i c out=""
  for (( i=0; i<${#s}; i++ )); do
    c="${s:i:1}"
    case "$c" in
      [a-zA-Z0-9.~_-]) out+="$c" ;;
      ' ') out+='+' ;;                            # cht.sh tolerates '+' for spaces
      *) printf -v out '%s%%%02X' "$out" "'$c" ;; # percent-encode
    esac
  done
  printf '%s' "$out"
}

# Build the path based on whether the selection is a language or a command
if grep -Fqx -- "$selected" "$LANGFILE"; then
  # Language cheat (e.g., "bash/for loop")
  path="cht.sh/$(urlencode "$selected")/$(urlencode "$query")"
else
  # Command cheat (e.g., "tar~extract")
  # cht.sh expects "cmd~topic" form
  path="cht.sh/$(urlencode "${selected}~${query}")"
fi

# Choose a viewer: bat if available, else less
viewer_cmd='if command -v bat >/dev/null 2>&1; then
  curl -sS "'"$path"'" | bat --style=plain --paging=always -l md
else
  curl -sS "'"$path"'" | less -R
fi'

# Open in a new tmux window and keep it open via the pager
tmux new-window -n "cht:${selected}" "bash -lc 'echo $path; echo; $viewer_cmd'"


# OLD CODE
# #!/usr/bin/env bash
# selected=`cat ~/.tmux-cht-languages ~/.tmux-cht-command | fzf`
# if [[ -z $selected ]]; then
#     exit 0
# fi
# 
# read -p "Enter Query: " query
# 
# if grep -qs "$selected" ~/.tmux-cht-languages; then
#     query=`echo $query | tr ' ' '+'`
#     tmux neww bash -c "echo \"curl cht.sh/$selected/$query/\" & curl cht.sh/$selected/$query & while [ : ]; do sleep 1; done"
# else
#     tmux neww bash -c "curl -s cht.sh/$selected~$query | bat --style=plain"
# fi
