#!/usr/bin/env bash
# Sync MCP servers from dotfiles to ~/.claude.json

set -euo pipefail

DOTFILES_MCP="$HOME/dots/.claude/mcp-servers.json"
CLAUDE_CONFIG="$HOME/.claude.json"

if [ ! -f "$DOTFILES_MCP" ]; then
    echo "Error: $DOTFILES_MCP not found"
    exit 1
fi

if [ ! -f "$CLAUDE_CONFIG" ]; then
    echo "Error: $CLAUDE_CONFIG not found"
    exit 1
fi

echo "Syncing MCP servers from dotfiles to ~/.claude.json..."

python3 << 'PYTHON_SCRIPT'
import json
import os
import sys

home = os.path.expanduser("~")
dotfiles_mcp_path = f"{home}/dots/.claude/mcp-servers.json"
claude_config_path = f"{home}/.claude.json"

# Read MCP servers from dotfiles
with open(dotfiles_mcp_path, 'r') as f:
    mcp_config = json.load(f)

# Read current Claude config
with open(claude_config_path, 'r') as f:
    claude_config = json.load(f)

# Update or add mcpServers section
claude_config['mcpServers'] = mcp_config['mcpServers']

# Write back to Claude config
with open(claude_config_path, 'w') as f:
    json.dump(claude_config, f, indent=2)

print(f"✓ Synced {len(mcp_config['mcpServers'])} MCP server(s) to {claude_config_path}")
for server_name in mcp_config['mcpServers'].keys():
    print(f"  - {server_name}")
PYTHON_SCRIPT

echo "Done! MCP servers are now synced."
