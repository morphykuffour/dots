# Hammerspoon Keybindings

## PaperWM - Tiling Window Manager

All PaperWM keybindings use the **Hyper** key combo: `Ctrl+Alt+Cmd` (⌃⌥⌘)

### Window Navigation

| Keybinding | Action |
|------------|--------|
| `⌃⌥⌘ + h` | Focus window to the left |
| `⌃⌥⌘ + j` | Focus window below |
| `⌃⌥⌘ + k` | Focus window above |
| `⌃⌥⌘ + l` | Focus window to the right |

### Window Movement

| Keybinding | Action |
|------------|--------|
| `⌃⌥⌘⇧ + h` | Swap window left |
| `⌃⌥⌘⇧ + j` | Swap window down |
| `⌃⌥⌘⇧ + k` | Swap window up |
| `⌃⌥⌘⇧ + l` | Swap window right |

### Window Sizing & Positioning

| Keybinding | Action |
|------------|--------|
| `⌃⌥⌘ + c` | Center window |
| `⌃⌥⌘ + f` | Full width |
| `⌃⌥⌘ + r` | Cycle width (1/3 → 1/2 → 2/3) |
| `⌃⌥⌘⇧ + r` | Cycle height (1/3 → 1/2 → 2/3) |
| `⌃⌥⌘ + Esc` | Toggle floating (remove from tiling) |

### Space Navigation

| Keybinding | Action |
|------------|--------|
| `⌃⌥⌘ + ,` | Switch to space on the left |
| `⌃⌥⌘ + .` | Switch to space on the right |
| `⌃⌥⌘ + 1` | Switch to space 1 |
| `⌃⌥⌘ + 2` | Switch to space 2 |
| `⌃⌥⌘ + 3` | Switch to space 3 |
| `⌃⌥⌘ + 4` | Switch to space 4 |
| `⌃⌥⌘ + 5` | Switch to space 5 |
| `⌃⌥⌘ + 6` | Switch to space 6 |
| `⌃⌥⌘ + 7` | Switch to space 7 |
| `⌃⌥⌘ + 8` | Switch to space 8 |
| `⌃⌥⌘ + 9` | Switch to space 9 |

### Move Window to Space

| Keybinding | Action |
|------------|--------|
| `⌃⌥⌘⇧ + 1` | Move window to space 1 and tile |
| `⌃⌥⌘⇧ + 2` | Move window to space 2 and tile |
| `⌃⌥⌘⇧ + 3` | Move window to space 3 and tile |
| `⌃⌥⌘⇧ + 4` | Move window to space 4 and tile |
| `⌃⌥⌘⇧ + 5` | Move window to space 5 and tile |
| `⌃⌥⌘⇧ + 6` | Move window to space 6 and tile |
| `⌃⌥⌘⇧ + 7` | Move window to space 7 and tile |
| `⌃⌥⌘⇧ + 8` | Move window to space 8 and tile |
| `⌃⌥⌘⇧ + 9` | Move window to space 9 and tile |

## Picture-in-Picture (PiP)

PiP windows are automatically positioned and sized when created:
- **Size:** 640×360 pixels
- **Position:** Bottom-right corner (20px from right, 100px from bottom)

Toggle PiP using Vimium keybinding: `,p`

## Hammerspoon System

| Keybinding | Action |
|------------|--------|
| `⌘⌥ + R` | Reload Hammerspoon config |
| `⌘⌥ + C` | Open Hammerspoon console |

## Configuration

### PaperWM Settings

```lua
PaperWM.window_gap = 8
PaperWM.window_ratios = { 1/3, 1/2, 2/3 }
```

### PiP Settings

```lua
PIP_WIDTH = 640
PIP_HEIGHT = 360
PIP_X_OFFSET = 20   -- pixels from right edge
PIP_Y_OFFSET = 100  -- pixels from bottom
```

## Setup Requirements

For PaperWM to work properly:

1. Open **System Settings** → **Desktop & Dock**
2. Scroll to "Mission Control" section
3. **Uncheck:** "Automatically rearrange Spaces based on most recent use"
4. **Check:** "Displays have separate Spaces"

## Files

- Config: `~/.hammerspoon/init.lua`
- This file: `~/.hammerspoon/KEYBINDINGS.md`
- PaperWM: `~/.hammerspoon/Spoons/PaperWM.spoon/`

## Tips

- Use `⌃⌥⌘ + Esc` to toggle a window in/out of the tiling system if it's misbehaving
- The window ratios can be cycled with `⌃⌥⌘ + r` for width and `⌃⌥⌘⇧ + r` for height
- PiP windows are automatically excluded from tiling
- Press `⌘⌥ + R` after editing `init.lua` to reload your config
