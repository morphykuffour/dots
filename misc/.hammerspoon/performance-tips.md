# Hammerspoon Performance Optimization Guide

## Additional System-Level Optimizations

### 1. **macOS Settings**
```bash
# Reduce transparency (less GPU work)
defaults write com.apple.universalaccess reduceTransparency -bool true

# Disable window animations
defaults write NSGlobalDomain NSAutomaticWindowAnimationsEnabled -bool false

# Faster key repeat (less lag feeling)
defaults write NSGlobalDomain KeyRepeat -int 2
defaults write NSGlobalDomain InitialKeyRepeat -int 15

# Disable Mission Control animations
defaults write com.apple.dock expose-animation-duration -float 0.1
```

### 2. **Hammerspoon Process Priority**
Create a launch agent to run Hammerspoon with higher priority:

```xml
<!-- ~/Library/LaunchAgents/com.hammerspoon.priority.plist -->
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>Label</key>
    <string>com.hammerspoon.priority</string>
    <key>ProgramArguments</key>
    <array>
        <string>renice</string>
        <string>-10</string>
        <string>-p</string>
        <string>$(pgrep Hammerspoon)</string>
    </array>
    <key>RunAtLoad</key>
    <true/>
</dict>
</plist>
```

### 3. **Memory Management**
Add to your Hammerspoon config:

```lua
-- Limit Hammerspoon memory usage
hs.timer.doEvery(300, function()  -- Every 5 minutes
    collectgarbage("collect")
    -- If memory usage is high, do a full collection
    if collectgarbage("count") > 50000 then  -- 50MB
        collectgarbage("collect")
        collectgarbage("collect")  -- Double collect for thorough cleanup
    end
end)
```

### 4. **Disable Unused macOS Features**
```bash
# Disable Spotlight indexing for faster IO
sudo mdutil -a -i off  # (only if you don't use Spotlight)

# Disable swap if you have enough RAM (16GB+)
sudo nvram boot-args="vm_compressor=2"  # Compress instead of swap
```

### 5. **Alternative Window Managers**
If Hammerspoon is still too slow, consider:

- **yabai**: Written in C, much faster but requires SIP disabled
- **Rectangle**: Native Swift app, very fast but less customizable
- **Amethyst**: Lightweight tiling WM

### 6. **Hammerspoon Alternatives for Specific Tasks**

Instead of using Hammerspoon for everything:

- **skhd**: For hotkeys only (much faster)
- **Karabiner-Elements**: For key remapping
- **BetterTouchTool**: For trackpad gestures
- **Alfred**: For app launching

### 7. **Profile and Debug**

Add this to find bottlenecks:

```lua
-- Performance profiler
local profile = false  -- Set to true to enable

if profile then
    local timers = {}
    
    local function timeIt(name, fn)
        local start = hs.timer.absoluteTime()
        fn()
        local duration = (hs.timer.absoluteTime() - start) / 1000000
        timers[name] = (timers[name] or 0) + duration
    end
    
    -- Wrap expensive operations
    local originalBind = hs.hotkey.bind
    hs.hotkey.bind = function(mods, key, fn)
        originalBind(mods, key, function()
            timeIt("hotkey:" .. table.concat(mods, "+") .. "+" .. key, fn)
        end)
    end
    
    -- Report every 60 seconds
    hs.timer.doEvery(60, function()
        print("=== Performance Report ===")
        for name, time in pairs(timers) do
            print(string.format("%s: %.2fms", name, time))
        end
        timers = {}  -- Reset
    end)
end
```

## Recommended Ultra-Fast Setup

For absolute minimum latency:

1. Use `init-ultra-optimized.lua`
2. Disable Hammerspoon menu bar icon
3. Use simple modifiers (Cmd+Alt instead of Hyper)
4. Only load modules you actively use
5. Run with increased priority
6. Disable window animations system-wide

## Testing Performance

```bash
# Check Hammerspoon CPU usage
top -pid $(pgrep Hammerspoon)

# Check response time
time osascript -e 'tell application "Hammerspoon" to execute lua code "return 1+1"'
```

The goal is < 10ms response time for any action.