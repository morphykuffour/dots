# Claude Code Configuration

This directory contains version-controlled Claude Code configurations managed with GNU Stow.

## Files

- `settings.local.json` - Local permissions and settings for Claude Code
- `mcp-servers.json` - MCP (Model Context Protocol) server configurations
- `sync-mcp.sh` - Script to sync MCP servers to `~/.claude.json`

## MCP Server Management

### Why separate MCP configuration?

The `~/.claude.json` file contains both:
1. **Dynamic state** - startup counts, tips, feature flags (changes frequently)
2. **MCP server configs** - your configured MCP servers (version controlled)

To version control only the MCP servers while allowing Claude to manage its internal state, we keep MCP configurations in `mcp-servers.json`.

### Adding a new MCP server

Option 1: Using Claude CLI (recommended):
```bash
# Add a server (it will be added to ~/.claude.json)
claude mcp add --transport stdio --scope user <name> -- <command>

# Then extract it to version control:
python3 << 'EOF'
import json
with open('~/.claude.json'.replace('~', '$HOME'), 'r') as f:
    config = json.load(f)
with open('~/dots/.claude/mcp-servers.json'.replace('~', '$HOME'), 'w') as f:
    json.dump({'mcpServers': config.get('mcpServers', {})}, f, indent=2)
EOF
```

Option 2: Edit `mcp-servers.json` directly:
```bash
# Edit the file
vim ~/dots/.claude/mcp-servers.json

# Then sync to Claude
~/dots/.claude/sync-mcp.sh
```

### Syncing MCP servers

After editing `mcp-servers.json`, run:
```bash
~/dots/.claude/sync-mcp.sh
```

This will merge your MCP server configurations into `~/.claude.json` without affecting Claude's internal state.

## Stow Setup

Since `~/.claude.json` is a mixed-use file, we don't stow it directly. Instead:

1. `settings.local.json` should be copied to `~/.claude/settings.local.json` (not stowed)
2. `mcp-servers.json` is version controlled here
3. Use `sync-mcp.sh` to apply MCP changes

To set up:
```bash
# Create .claude directory if it doesn't exist
mkdir -p ~/.claude

# Copy settings
cp ~/dots/.claude/settings.local.json ~/.claude/

# Sync MCP servers
chmod +x ~/dots/.claude/sync-mcp.sh
~/dots/.claude/sync-mcp.sh
```

## Configured MCP Servers

### Puppeteer
- **Description**: Browser automation and web scraping with Puppeteer and Chromium
- **Transport**: stdio
- **Command**: `npx -y puppeteer-mcp-server`
- **Scope**: User (available in all projects)

**Capabilities**:
- Navigate to URLs and interact with pages
- Take screenshots
- Click elements, fill forms, hover
- Execute JavaScript in page context
- Capture console logs
- Automate multi-step browser workflows

**Usage example**:
```
> Use Puppeteer to scrape the prices from example.com
> Take a screenshot of reddit.com in mobile viewport
> Navigate to github.com and click the login button
```

## Resources

- [Claude Code MCP Docs](https://code.claude.com/docs/en/mcp)
- [MCP Registry](https://api.anthropic.com/mcp-registry/docs)
- [Puppeteer MCP Server](https://github.com/merajmehrabi/puppeteer-mcp-server)
