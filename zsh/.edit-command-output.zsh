# Edit Command Output — Alt+e
# Runs the current command line, captures output, opens in $EDITOR.
# On :wq the edited text replaces BUFFER so you can pipe/use it.
# Commands with no output (like ln, mv, cp) execute normally and clear BUFFER.

edit-command-output() {
    local cmd="$BUFFER"
    [[ -z "${cmd// }" ]] && return 0

    local tmpf="${TMPDIR:-/tmp}/zsh-eco-$$.txt"
    local errf="${TMPDIR:-/tmp}/zsh-eco-err-$$.txt"
    local exit_code

    # Run command, capture stdout and stderr separately
    eval "$cmd" > "$tmpf" 2> "$errf"
    exit_code=$?

    if (( exit_code != 0 )); then
        # Command failed - clear buffer and show brief error, discard stderr
        command rm -f "$tmpf" "$errf"
        BUFFER=""
        CURSOR=0
        zle reset-prompt
        return 0
    fi

    # Command succeeded - discard stderr
    command rm -f "$errf"

    # If command succeeded but produced no output, just clear the buffer
    # This handles commands like ln, mv, cp, mkdir, etc.
    if [[ ! -s "$tmpf" ]]; then
        command rm -f "$tmpf"
        BUFFER=""
        CURSOR=0
        zle reset-prompt
        return 0
    fi

    # Open editor on the captured output
    exec </dev/tty
    "${EDITOR:-vim}" "$tmpf"

    BUFFER="$(<$tmpf)"
    CURSOR=${#BUFFER}
    command rm -f "$tmpf"
    zle reset-prompt
}

zle -N edit-command-output

bindkey '^[e' edit-command-output
bindkey -M vicmd '^[e' edit-command-output
