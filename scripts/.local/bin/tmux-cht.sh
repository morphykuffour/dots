#!/usr/bin/env bash
selected=`cat ~/.tmux-cht-languages ~/.tmux-cht-command | fzf`
if [[ -z $selected ]]; then
    exit 0
fi

read -p "Enter Query: " query

if grep -qs "$selected" ~/.tmux-cht-languages; then
    query=`echo $query | tr ' ' '+'`
    tmux neww bash -c "echo \"curl cht.sh/$selected/$query/\" & curl cht.sh/$selected/$query & while [ : ]; do sleep 1; done"
else
    tmux neww bash -c "curl -s cht.sh/$selected~$query | less"
fi

# curl cht.sh/{language}/query+string
# curl cht.sh/{core-util}~{operation}

# curl cht.sh/golang/learn:slice

# languages=`echo "golang lua cpp c typescript nodejs" | tr ' ' '\n'`
# core_utils=`echo "xargs find mv sed awk" | tr ' ' '\n'`

