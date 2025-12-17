#!/usr/bin/env zsh
# Verification script to test that all zsh plugins are loaded correctly

echo "=== ZSH Plugin Verification Script ==="
echo ""

# Wait a moment for deferred plugins to load
sleep 1

# Test 1: Check if zsh-defer is loaded
if (( $+functions[zsh-defer] )); then
    echo "✓ zsh-defer is loaded"
else
    echo "✗ zsh-defer is NOT loaded"
fi

# Test 2: Check if autosuggestions is loaded
if typeset -f | grep -q "_zsh_autosuggest"; then
    echo "✓ zsh-autosuggestions is loaded"
else
    echo "✗ zsh-autosuggestions is NOT loaded"
fi

# Test 3: Check if syntax highlighting is loaded
if typeset -f | grep -q "fast-syntax-highlighting"; then
    echo "✓ fast-syntax-highlighting is loaded"
else
    echo "✗ fast-syntax-highlighting is NOT loaded"
fi

# Test 4: Check if system clipboard is loaded
if typeset -f | grep -q "zsh-system-clipboard"; then
    echo "✓ zsh-system-clipboard is loaded"
else
    echo "✗ zsh-system-clipboard is NOT loaded"
fi

# Test 5: Check deferred task status
if (( ${#_zsh_defer_tasks[@]} == 0 )); then
    echo "✓ All deferred tasks have been executed"
else
    echo "⚠ ${#_zsh_defer_tasks[@]} tasks still pending (may need more time)"
fi

echo ""
echo "=== Test Instructions ==="
echo "1. Type a command like 'ls' - you should see:"
echo "   - Syntax highlighting (commands in different colors)"
echo "   - Autosuggestions (gray text appearing as you type)"
echo ""
echo "2. Try using autojump by typing: j <partial-directory-name>"
echo ""
echo "=== Performance ==="
echo "Shell startup time should be under 0.5 seconds"
echo "Plugins load asynchronously after prompt appears"
