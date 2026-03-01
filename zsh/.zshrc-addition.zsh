# ======================= EDIT COMMAND OUTPUT WIDGET =======================
# ZLE widget that executes current command, captures stdout, and edits it in Neovim
# 
# Keybindings:
# - Ctrl+e: edit-command-line (existing behavior preserved)  
# - Ctrl+Shift+e: edit_current_command_output (new feature)
#   Common escape sequences: ^[[69;5u (kitty), ^[^E (some terminals)
#   Fallback: Alt+e

edit_current_command_output() {
    emulate -L zsh
    setopt local_options extended_glob
    
    local buffer="$BUFFER"
    local temp_file
    local exit_status
    
    # Check if buffer is empty or only whitespace
    if [[ -z "${buffer// }" ]]; then
        return 0
    fi
    
    # Create secure temporary file with .sh suffix for syntax highlighting
    if command -v mktemp >/dev/null 2>&1; then
        temp_file=$(mktemp "${TMPDIR:-/tmp}/zsh-edit-output.XXXXXX.sh")
    else
        # Fallback for systems without mktemp
        temp_file="${TMPDIR:-/tmp}/zsh-edit-output.$$.$RANDOM.sh"
        touch "$temp_file" 2>/dev/null
    fi
    
    if [[ ! -f "$temp_file" ]]; then
        print -r "Error: Could not create temporary file" >&2
        return 1
    fi
    
    # Ensure cleanup on exit
    trap "rm -f '$temp_file' 2>/dev/null" EXIT INT TERM
    
    # Execute the command and capture stdout, let stderr pass through
    {
        eval "$buffer" >"$temp_file"
        exit_status=$?
    }
    
    # Handle command failure
    if (( exit_status != 0 )); then
        print -r "Command failed (exit $exit_status): $buffer" >&2
        rm -f "$temp_file" 2>/dev/null
        return 0  # Don't change BUFFER/CURSOR on failure
    fi
    
    # Open in Neovim with shell filetype for syntax highlighting and completion
    nvim +"setfiletype sh" "$temp_file"
    
    # Read the edited content back into BUFFER
    BUFFER=$(<"$temp_file")
    
    # Position cursor at the end
    CURSOR=${#BUFFER}
    
    # Clean up
    rm -f "$temp_file" 2>/dev/null
    trap - EXIT INT TERM
}

# Register the widget
zle -N edit_current_command_output

# Preserve existing edit-command-line functionality on Ctrl+e
autoload -Uz edit-command-line
zle -N edit-command-line

# Bind Ctrl+e to edit-command-line in both insert and vicmd modes
bindkey '^e' edit-command-line
bindkey -M vicmd '^e' edit-command-line

# ======================= IMPROVED KEYBINDINGS =======================
# Multiple keybinding options for maximum compatibility across terminals

# Primary binding: Alt+o (most reliable across terminals)
bindkey '^[o' edit_current_command_output
bindkey -M vicmd '^[o' edit_current_command_output

# Secondary binding: Ctrl+Alt+e (for terminals that support it)  
bindkey '^[^E' edit_current_command_output
bindkey -M vicmd '^[^E' edit_current_command_output

# Kitty terminal specific bindings
if [[ "$TERM" == *"kitty"* ]]; then
    # Ctrl+Shift+e in kitty (CSI u encoding)
    bindkey '^[[69;5u' edit_current_command_output
    bindkey -M vicmd '^[[69;5u' edit_current_command_output
    
    # Alternative kitty binding
    bindkey '^[[1;6E' edit_current_command_output
    bindkey -M vicmd '^[[1;6E' edit_current_command_output
fi

# Additional terminal-specific bindings
case "$TERM" in
    *xterm*|*rxvt*)
        # xterm-style Ctrl+Shift+e
        bindkey '^[[1;6E' edit_current_command_output
        bindkey -M vicmd '^[[1;6E' edit_current_command_output
        ;;
    *screen*|*tmux*)
        # tmux/screen compatible
        bindkey '^[[1;6E' edit_current_command_output
        bindkey -M vicmd '^[[1;6E' edit_current_command_output
        ;;
esac

# Fallback: Alt+e (if Alt+o conflicts with anything)
bindkey '^[e' edit_current_command_output
bindkey -M vicmd '^[e' edit_current_command_output

# ======================= HELPER FUNCTIONS =======================

# Function to detect the actual key sequence in your terminal
detect_ctrl_shift_e() {
    print "Press Ctrl+Shift+e to detect the escape sequence..."
    print "Press Ctrl+C to cancel"
    print ""
    
    # Read a single key sequence
    local key
    read -k key
    
    # Convert to hex for analysis
    local hex_sequence
    hex_sequence=$(printf "%s" "$key" | xxd -p)
    
    print "\nDetected sequence: $key"
    print "Hex representation: $hex_sequence" 
    print "\nTo bind this sequence, add:"
    print "bindkey '$(printf "%q" "$key")' edit_current_command_output"
}

# Test all keybindings
test_keybindings() {
    print "Testing keybindings for edit_current_command_output:"
    print ""
    
    local bindings=(
        "^[o:Alt+o (primary)"
        "^[^E:Ctrl+Alt+e" 
        "^[e:Alt+e (fallback)"
    )
    
    # Add terminal-specific bindings
    if [[ "$TERM" == *"kitty"* ]]; then
        bindings+=("^[[69;5u:Ctrl+Shift+e (kitty CSI u)")
        bindings+=("^[[1;6E:Ctrl+Shift+e (kitty xterm)")
    fi
    
    case "$TERM" in
        *xterm*|*rxvt*|*screen*|*tmux*)
            bindings+=("^[[1;6E:Ctrl+Shift+e (${TERM})")
            ;;
    esac
    
    for binding in "${bindings[@]}"; do
        local seq="${binding%%:*}"
        local desc="${binding#*:}"
        
        if bindkey | grep -q "$(printf '%q' "$seq")"; then
            print "✓ $desc: $(printf '%q' "$seq")"
        else
            print "✗ $desc: $(printf '%q' "$seq") - NOT BOUND"
        fi
    done
    
    print ""
    print "Current bindings:"
    bindkey | grep edit_current_command_output
}