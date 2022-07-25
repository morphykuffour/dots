join-lines() {
  local item
  while read item; do
    echo -n "${(q)item} "
  done
}

bind-git-helper() {
  local char
  for c in $@; do
    eval "fzf-g$c-widget() { local result=\$(${FZF_PREFIX}g$c | join-lines); zle reset-prompt; LBUFFER+=\$result }"
    eval "zle -N fzf-g$c-widget"
    eval "bindkey '^g^$c' fzf-g$c-widget"
  done
}
# need this for git things to work
bindkey -r "^G"bindkey -r "^G"
bind-git-helper s b t r l
unset -f bind-git-helper

## KEYMAPS FOR GIT ZSH
# C-g C-f git status
# C-g C-b git branch
# C-g C-t git tag
# C-g C-r git remote 
# C-g C-h git log
