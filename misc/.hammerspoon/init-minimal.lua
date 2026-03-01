-- MINIMAL HAMMERSPOON CONFIG FOR MAXIMUM PERFORMANCE
-- This version prioritizes speed over features

-- Disable ALL animations
hs.window.animationDuration = 0

-- Enable IPC for CLI access (if needed)
require("hs.ipc")

-- ============================================================================
-- PaperWM - Minimal Configuration
-- ============================================================================

PaperWM = hs.loadSpoon("PaperWM")

-- Minimal settings
PaperWM.window_gap = 8
PaperWM.center_mouse = false

-- Filter out unnecessary apps to reduce processing
PaperWM.window_filter = hs.window.filter.new()
PaperWM.window_filter:setAppFilter('Finder', false)
PaperWM.window_filter:setAppFilter('System Preferences', false)
PaperWM.window_filter:setAppFilter('System Settings', false)
PaperWM.window_filter:setAppFilter('Activity Monitor', false)
PaperWM.window_filter:setAppFilter('Console', false)
PaperWM.window_filter:setAppFilter('Archive Utility', false)

-- Only essential hotkeys
PaperWM:bindHotkeys({
    focus_left  = {{"cmd", "alt"}, "h"},
    focus_right = {{"cmd", "alt"}, "l"},
    focus_up    = {{"cmd", "alt"}, "k"},
    focus_down  = {{"cmd", "alt"}, "j"},
    
    swap_left  = {{"cmd", "alt", "shift"}, "h"},
    swap_right = {{"cmd", "alt", "shift"}, "l"},
    
    full_width = {{"cmd", "alt"}, "f"},
    cycle_width = {{"cmd", "alt"}, "r"},
})

PaperWM:start()

-- Manual reload (Hyper+R)
hs.hotkey.bind({"shift", "ctrl", "alt", "cmd"}, "r", hs.reload)

-- Silent load
print("Hammerspoon minimal config loaded")