-- Enable IPC for CLI access
require("hs.ipc")

-- ============================================================================
-- Performance Optimizations
-- ============================================================================

-- Disable animations for faster switching
hs.window.animationDuration = 0

-- Reduce event settle delays
local eventDelay = 0.05  -- reduced from 0.15

-- ============================================================================
-- PIP Window Positioning (Optimized)
-- ============================================================================

local PIP_WIDTH = 640
local PIP_HEIGHT = 360
local PIP_X_OFFSET = 20
local PIP_Y_OFFSET = 100

-- Only watch for PiP windows, not all windows
pipWatcher = hs.window.filter.new(function(win)
    local title = win:title() or ""
    return title:match("Picture") or win:subrole() == "AXFloatingWindow"
end)

pipWatcher:subscribe(hs.window.filter.windowCreated, function(window)
    -- Reduced delay for faster positioning
    hs.timer.doAfter(0.05, function()
        local screen = window:screen()
        if not screen then return end
        local screenFrame = screen:frame()

        local x = screenFrame.x + screenFrame.w - PIP_WIDTH - PIP_X_OFFSET
        local y = screenFrame.y + screenFrame.h - PIP_HEIGHT - PIP_Y_OFFSET

        window:setFrame({x = x, y = y, w = PIP_WIDTH, h = PIP_HEIGHT})
    end)
end)

-- ============================================================================
-- PaperWM - Tiling Window Manager (Optimized)
-- ============================================================================

PaperWM = hs.loadSpoon("PaperWM")

-- Disable features that slow down switching
PaperWM.window_gap = 8
PaperWM.window_ratios = { 1/3, 1/2, 2/3 }
PaperWM.center_mouse = false  -- Disable mouse centering for speed

-- Filter out apps that don't need tiling (reduces overhead)
PaperWM.window_filter = hs.window.filter.new()
PaperWM.window_filter:setAppFilter('Finder', false)
PaperWM.window_filter:setAppFilter('System Preferences', false)
PaperWM.window_filter:setAppFilter('System Settings', false)
PaperWM.window_filter:setAppFilter('Activity Monitor', false)

-- Bind only essential hotkeys to reduce event processing
PaperWM:bindHotkeys({
    -- Focus switching
    focus_left  = {{"shift", "ctrl", "alt", "cmd"}, "h"},
    focus_right = {{"shift", "ctrl", "alt", "cmd"}, "l"},
    focus_up    = {{"shift", "ctrl", "alt", "cmd"}, "k"},
    focus_down  = {{"shift", "ctrl", "alt", "cmd"}, "j"},

    -- Window swapping
    swap_left  = {{"ctrl", "alt", "cmd"}, "h"},
    swap_right = {{"ctrl", "alt", "cmd"}, "l"},
    
    -- Essential window controls
    full_width    = {{"shift", "ctrl", "alt", "cmd"}, "f"},
    cycle_width   = {{"shift", "ctrl", "alt", "cmd"}, "r"},
    toggle_floating = {{"shift", "ctrl", "alt", "cmd"}, "escape"},

    -- Space switching (only 1-5 for performance)
    switch_space_1 = {{"shift", "ctrl", "alt", "cmd"}, "1"},
    switch_space_2 = {{"shift", "ctrl", "alt", "cmd"}, "2"},
    switch_space_3 = {{"shift", "ctrl", "alt", "cmd"}, "3"},
    switch_space_4 = {{"shift", "ctrl", "alt", "cmd"}, "4"},
    switch_space_5 = {{"shift", "ctrl", "alt", "cmd"}, "5"},
})

PaperWM:start()

-- ============================================================================
-- DISABLED/OPTIMIZED FEATURES FOR PERFORMANCE
-- ============================================================================

-- ActiveSpace - DISABLED (menu bar updates slow down switching)
-- Uncomment only if you really need it
-- ActiveSpace = hs.loadSpoon("ActiveSpace")
-- ActiveSpace.compact = true
-- ActiveSpace:start()

-- WarpMouse - DISABLED (mouse movement adds latency)
-- Uncomment only if you really need it
-- WarpMouse = hs.loadSpoon("WarpMouse")
-- WarpMouse.margin = 8
-- WarpMouse:start()

-- Swipe gestures - DISABLED (continuous event processing)
-- Trackpad swipes add significant overhead

-- FocusMode - DISABLED by default (overlay rendering is expensive)
-- Only load if explicitly needed
if false then  -- Change to true to enable
    FocusMode = hs.loadSpoon("FocusMode")
    FocusMode.dimAlpha = 0.5
    FocusMode.eventSettleDelay = eventDelay
    hs.hotkey.bind({"shift", "ctrl", "alt", "cmd"}, "d", function()
        if FocusMode.enabled then
            FocusMode:exit()
        else
            FocusMode:enter()
        end
    end)
end

-- Window Focus Highlight - LIGHTWEIGHT VERSION
-- Uses simple border instead of canvas for performance
local focusBorder = nil

local function updateFocusBorder()
    if focusBorder then
        focusBorder:delete()
        focusBorder = nil
    end
    
    -- Only create border if explicitly enabled
    if hs.settings.get("showFocusBorder") == false then return end
    
    local win = hs.window.focusedWindow()
    if not win then return end

    local frame = win:frame()
    focusBorder = hs.drawing.rectangle(frame)
    focusBorder:setStrokeColor({hex = "#FFDB00", alpha = 0.8})
    focusBorder:setStrokeWidth(3)
    focusBorder:setFill(false)
    focusBorder:setLevel(hs.drawing.windowLevels.overlay)
    focusBorder:show()
end

-- Only update on focus change, not on every window movement
hs.window.filter.new():setDefaultFilter():subscribe(
    hs.window.filter.windowFocused, 
    updateFocusBorder
)

-- ============================================================================
-- Emacs focus - Optimized
-- ============================================================================

-- Only watch for Emacs if it's installed
if hs.application.get("Emacs") then
    hs.application.watcher.new(function(appName, eventType, appObject)
        if appName == "Emacs" and eventType == hs.application.watcher.launched then
            -- Reduced delay
            hs.timer.doAfter(0.1, function()
                appObject:activate()
            end)
        end
    end):start()
end

-- ============================================================================
-- Config reload - ONLY on manual save
-- ============================================================================

-- Disable automatic reload to prevent interruptions
-- Uncomment to enable auto-reload
--[[
hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", function(files)
    for _, file in pairs(files) do
        if file:match("init.lua$") then
            hs.reload()
        end
    end
end):start()
--]]

-- Manual reload hotkey
hs.hotkey.bind({"shift", "ctrl", "alt", "cmd"}, "0", function()
    hs.reload()
end)

-- Minimal alert
hs.alert.show("Hammerspoon loaded", 0.5)