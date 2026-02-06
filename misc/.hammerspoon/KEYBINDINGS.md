# Hammerspoon Configuration - Keybindings and Usage Guide

This document describes all the keybindings and features in this Hammerspoon configuration, including the PaperWM tiling window manager.

## Table of Contents

- [Overview](#overview)
- [PaperWM Window Manager](#paperwm-window-manager)
- [All Keybindings](#all-keybindings)
  - [Window Navigation](#window-navigation)
  - [Window Movement](#window-movement)
  - [Window Sizing](#window-sizing)
  - [Window Management](#window-management)
  - [Space Switching](#space-switching)
  - [Window to Space](#window-to-space)
  - [Focus Mode](#focus-mode)
- [Additional Features](#additional-features)
- [Configuration Options](#configuration-options)

---

## Overview

This Hammerspoon configuration provides:

- **PaperWM**: Horizontal tiling window manager inspired by the PaperWM Gnome extension
- **FocusMode**: Dims everything except the focused window
- **ActiveSpace**: Shows the active Mission Control space in the menu bar
- **WarpMouse**: Moves the mouse cursor between screen edges
- **Swipe Gestures**: Navigate between windows using trackpad gestures
- **Window Focus Highlight**: Yellow border around the focused window
- **Auto-positioning**: Automatically positions Picture-in-Picture windows

All PaperWM keybindings use the **Hyper key** as the base modifier: `Shift + Ctrl + Alt + Cmd`

**Modifier Key Legend:**
- **Hyper** = `Shift + Ctrl + Alt + Cmd` (base actions: navigation, resizing, spaces)
- **Ctrl + Alt + Cmd** = Secondary modifier (swap/move actions)

---

## PaperWM Window Manager

PaperWM tiles windows horizontally in a scrollable layout. Windows are arranged in columns, and you can navigate between them using vim-style keybindings.

### Key Concepts

- **Tiling Layer**: Windows are automatically tiled horizontally
- **Floating Windows**: Windows can be toggled to float above the tiling layer
- **Spaces**: Each Mission Control space has its own independent tiling layout
- **Window Gaps**: 8px gap between windows (configurable)
- **Window Ratios**: Cycle through 1/3, 1/2, 2/3 width ratios

### Basic Workflow

1. Windows are automatically tiled as you open them
2. Navigate between windows using vim-style keybindings (h/j/k/l)
3. Rearrange windows by swapping them in the grid
4. Resize windows by cycling through preset width ratios
5. Toggle windows between tiling and floating modes
6. Move windows between Mission Control spaces

---

## All Keybindings

### Window Navigation

Navigate between windows in the tiling grid using vim-style keybindings.

| Keybinding | Action | Description |
|------------|--------|-------------|
| `Hyper + H` | Focus Left | Move focus to the window on the left |
| `Hyper + L` | Focus Right | Move focus to the window on the right |
| `Hyper + K` | Focus Up | Move focus to the window above |
| `Hyper + J` | Focus Down | Move focus to the window below |
| `Hyper + N` | Cycle Next | Move focus to the next window sequentially |
| `Hyper + P` | Cycle Previous | Move focus to the previous window sequentially |

**Alternative: 3-Finger Swipe Gestures** (uses "natural" scrolling direction)
- Swipe Left: Focus right window
- Swipe Right: Focus left window
- Swipe Up: Focus down window
- Swipe Down: Focus up window

### Window Movement

Move windows around in the tiling grid. Uses lighter modifier without Shift.

| Keybinding | Action | Description |
|------------|--------|-------------|
| `Ctrl + Alt + Cmd + H` | Swap Left | Move window to the left |
| `Ctrl + Alt + Cmd + L` | Swap Right | Move window to the right |
| `Ctrl + Alt + Cmd + K` | Swap Up | Move window up in its column |
| `Ctrl + Alt + Cmd + J` | Swap Down | Move window down in its column |

### Window Sizing

Adjust the size of the focused window.

| Keybinding | Action | Description |
|------------|--------|-------------|
| `Hyper + C` | Center Window | Center the window on screen |
| `Hyper + F` | Full Width | Toggle window to full screen width |
| `Hyper + R` | Cycle Width | Cycle through width ratios (1/3, 1/2, 2/3) |
| `Ctrl + Alt + Cmd + R` | Cycle Height | Cycle through height ratios |

### Window Management

Control window behavior and floating state.

| Keybinding | Action | Description |
|------------|--------|-------------|
| `Hyper + Escape` | Toggle Floating | Move window into/out of the tiling layer |

### Space Switching

Switch between Mission Control spaces.

| Keybinding | Action | Description |
|------------|--------|-------------|
| `Hyper + ,` | Previous Space | Switch to the space on the left |
| `Hyper + .` | Next Space | Switch to the space on the right |
| `Hyper + 1` | Space 1 | Jump to Mission Control space 1 |
| `Hyper + 2` | Space 2 | Jump to Mission Control space 2 |
| `Hyper + 3` | Space 3 | Jump to Mission Control space 3 |
| `Hyper + 4` | Space 4 | Jump to Mission Control space 4 |
| `Hyper + 5` | Space 5 | Jump to Mission Control space 5 |
| `Hyper + 6` | Space 6 | Jump to Mission Control space 6 |
| `Hyper + 7` | Space 7 | Jump to Mission Control space 7 |
| `Hyper + 8` | Space 8 | Jump to Mission Control space 8 |
| `Hyper + 9` | Space 9 | Jump to Mission Control space 9 |

### Window to Space

Move the focused window to a different Mission Control space.

| Keybinding | Action | Description |
|------------|--------|-------------|
| `Ctrl + Alt + Cmd + ,` | Move Window Left | Move window to previous space |
| `Ctrl + Alt + Cmd + .` | Move Window Right | Move window to next space |
| `Ctrl + Alt + Cmd + 1` | Move to Space 1 | Move window to space 1 and tile |
| `Ctrl + Alt + Cmd + 2` | Move to Space 2 | Move window to space 2 and tile |
| `Ctrl + Alt + Cmd + 3` | Move to Space 3 | Move window to space 3 and tile |
| `Ctrl + Alt + Cmd + 4` | Move to Space 4 | Move window to space 4 and tile |
| `Ctrl + Alt + Cmd + 5` | Move to Space 5 | Move window to space 5 and tile |
| `Ctrl + Alt + Cmd + 6` | Move to Space 6 | Move window to space 6 and tile |
| `Ctrl + Alt + Cmd + 7` | Move to Space 7 | Move window to space 7 and tile |
| `Ctrl + Alt + Cmd + 8` | Move to Space 8 | Move window to space 8 and tile |
| `Ctrl + Alt + Cmd + 9` | Move to Space 9 | Move window to space 9 and tile |

### Focus Mode

| Keybinding | Action | Description |
|------------|--------|-------------|
| `Hyper + D` | Toggle Focus Mode | Dim everything except focused window |

---

## Additional Features

### Window Focus Highlight

A yellow border (4px, color: #FFDB00) automatically appears around the focused window. The border:
- Updates when focus changes
- Moves with the window
- Disappears when window is unfocused
- Works across all spaces

### Picture-in-Picture Auto-Position

Picture-in-Picture windows from Chrome, Safari, and Firefox are automatically:
- Resized to 640x360 pixels
- Positioned 20px from the right edge
- Positioned 100px from the bottom

### WarpMouse

Mouse cursor automatically warps between screen edges with an 8px margin (matches PaperWM window gap).

### ActiveSpace Menu Bar

Displays the current Mission Control space number in the menu bar. Uncomment `ActiveSpace.compact = true` in init.lua for compact mode.

### Swipe Gesture Navigation

Use 3-finger swipes on the trackpad to navigate between windows:
- Threshold: 20% of trackpad size
- Uses "natural" scrolling direction
- Triggers once per swipe

### Emacs Auto-Focus

Emacs automatically receives focus 0.5 seconds after being launched.

### Auto-Reload

Configuration automatically reloads when `init.lua` is saved.

---

## Configuration Options

### Window Gaps

Edit in init.lua (line 118):

```lua
PaperWM.window_gap = 8  -- uniform gap on all sides
```

Or specify per-side:

```lua
PaperWM.window_gap = {top=8, bottom=8, left=8, right=8}
```

### Window Ratios

Edit in init.lua (line 121):

```lua
PaperWM.window_ratios = { 1/3, 1/2, 2/3 }  -- ratios cycled through with Ctrl+Alt+Cmd+R
```

### Center Mouse on Space Switch

Edit in init.lua (line 124):

```lua
PaperWM.center_mouse = true  -- center mouse on window after switching spaces
```

### Application Filtering

Exclude specific applications from tiling (uncomment and edit in init.lua, lines 127-128):

```lua
PaperWM.window_filter = hs.window.filter.new()
PaperWM.window_filter:setAppFilter('Finder', false)  -- exclude Finder
```

### Screen Filtering

Exclude specific screens from tiling (uncomment and edit in init.lua, lines 131-133):

```lua
PaperWM.screen_filter = {
    [hs.screen.primaryScreen():id()] = true,  -- only tile on primary screen
}
```

### Focus Mode Settings

Edit in init.lua (lines 101-104):

```lua
FocusMode.dimAlpha = 0.5              -- darkness (0.0-1.0)
FocusMode.mouseDim = true             -- dim app under mouse cursor
FocusMode.windowCornerRadius = 10     -- rounded corners for window holes
FocusMode.eventSettleDelay = 0.15     -- delay for smoother PaperWM integration
```

### Window Focus Border

Edit in init.lua (lines 232-233):

```lua
local focusBorderColor = {hex = "#FFDB00", alpha = 1}  -- yellow
local focusBorderWidth = 4  -- border thickness in pixels
```

### Picture-in-Picture Settings

Edit in init.lua (lines 3-6):

```lua
local PIP_WIDTH = 640
local PIP_HEIGHT = 360
local PIP_X_OFFSET = 20   -- pixels from right edge
local PIP_Y_OFFSET = 100  -- pixels from bottom
```

---

## Additional PaperWM Actions

These actions are available but not bound to keys by default. To enable them, uncomment the corresponding lines in init.lua (lines 191-193):

### Slurp and Barf

```lua
slurp_in = {{"ctrl", "alt", "cmd"}, "i"},  -- absorb window into column
barf_out = {{"ctrl", "alt", "cmd"}, "o"},  -- expel window from column
```

### Cycle Layout

```lua
cycle_layout = {{"ctrl", "alt", "cmd"}, "tab"},  -- cycle column layout
```

---

## System Requirements

- macOS with Mission Control
- "Displays have separate Spaces" must be enabled in System Preferences → Mission Control
- Arrange screens vertically to prevent windows from bleeding into other screens

---

## Troubleshooting

### Windows Not Tiling

1. Check that PaperWM is started: `PaperWM:start()` should be called in init.lua
2. Verify "Displays have separate Spaces" is enabled in Mission Control preferences
3. Reload configuration: save init.lua or run `hs.reload()` in the Hammerspoon console

### Keybindings Not Working

1. Ensure Hammerspoon has Accessibility permissions in System Preferences → Security & Privacy → Privacy → Accessibility
2. Check for conflicting keybindings with other applications
3. Verify the keybinding definition in init.lua (lines 136-194)

### Windows Bleeding Between Screens

Arrange screens vertically in System Preferences → Displays → Arrangement. MacOS does not allow windows to be fully off-screen, so horizontal screen arrangements can cause issues.

---

## Quick Reference Card

### Most Common Keybindings

**Hyper = Shift + Ctrl + Alt + Cmd**

```
Navigation:     Hyper + H/J/K/L
Move Windows:   Ctrl+Alt+Cmd + H/J/K/L
Cycle Windows:  Hyper + N/P
Resize:         Hyper + R (width), Ctrl+Alt+Cmd + R (height)
Center:         Hyper + C
Full Width:     Hyper + F
Float:          Hyper + Escape
Spaces:         Hyper + ,/. or 1-9
Move to Space:  Ctrl+Alt+Cmd + ,/. or 1-9
Focus Mode:     Hyper + D
```

---

Generated for Hammerspoon configuration at: `/Users/morph/dots/misc/.hammerspoon/`
