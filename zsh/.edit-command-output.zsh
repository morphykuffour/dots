# Edit Command Output — Alt+e
# Runs the current command line, captures output, opens in $EDITOR.
# On :wq the edited text replaces BUFFER so you can pipe/use it.

edit-command-output() {
    local cmd="$BUFFER"
    [[ -z "${cmd// }" ]] && return 0

    local tmpf="${TMPDIR:-/tmp}/zsh-eco-$$.txt"

    # Run command, capture stdout (stderr passes through to terminal)
    if ! eval "$cmd" > "$tmpf" 2>/dev/tty; then
        print -u2 "edit-command-output: command failed: $cmd"
        command rm -f "$tmpf"
        return 0
    fi

    # Bail if nothing was captured
    if [[ ! -s "$tmpf" ]]; then
        command rm -f "$tmpf"
        return 0
    fi

    # Open editor on the captured output
    exec </dev/tty
    "${EDITOR:-vim}" "$tmpf"

    BUFFER="$(<$tmpf)"
    CURSOR=${#BUFFER}
    command rm -f "$tmpf"
}

zle -N edit-command-output

bindkey '^[e' edit-command-output
bindkey -M vicmd '^[e' edit-command-output
