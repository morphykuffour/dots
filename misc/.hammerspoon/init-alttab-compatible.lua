-- HAMMERSPOON CONFIG OPTIMIZED FOR ALTTAB APP COMPATIBILITY
-- Minimal window management that doesn't interfere with AltTab

-- ============================================================================
-- CORE PERFORMANCE SETTINGS
-- ============================================================================

-- Disable ALL animations
hs.window.animationDuration = 0

-- Disable all logging
hs.window.filter.setLogLevel(0)
hs.hotkey.setLogLevel(0)
hs.timer.setLogLevel(0)
hs.logger.setGlobalLogLevel(0)

-- Hide menu bar icon to save resources
hs.menuIcon(false)

-- ============================================================================
-- IMPORTANT: DISABLE FEATURES THAT CONFLICT WITH ALTTAB
-- ============================================================================

-- NO window watchers (AltTab handles this)
-- NO window focus borders (causes lag with AltTab)
-- NO automatic window switching (let AltTab handle it)
-- NO PIP auto-positioning (can interfere with AltTab's window detection)

-- ============================================================================
-- LAZY LOADING PAPERWM (Minimal Integration)
-- ============================================================================

local paperWMLoaded = false
local paperWMActions = {}

local function loadPaperWM()
    if paperWMLoaded then return paperWMActions end
    paperWMLoaded = true
    
    PaperWM = hs.loadSpoon("PaperWM")
    
    -- Minimal config - let AltTab handle most window operations
    PaperWM.window_gap = 8
    PaperWM.center_mouse = false
    
    -- IMPORTANT: Exclude AltTab from being managed
    PaperWM.window_filter = hs.window.filter.new()
    PaperWM.window_filter:setAppFilter('AltTab', false)
    
    -- Also exclude system apps and utilities
    local excludeApps = {
        'AltTab', 'Hammerspoon', 'Finder', 'System Preferences',
        'System Settings', 'Activity Monitor', 'Console',
        'Screenshot', 'Control Room', 'Simulator',
        'Archive Utility', 'Calculator', 'Dictionary'
    }
    
    for _, app in ipairs(excludeApps) do
        PaperWM.window_filter:setAppFilter(app, false)
    end
    
    PaperWM:start()
    
    -- Cache only essential actions
    local actions = PaperWM.actions.actions()
    paperWMActions = {
        -- Only spatial arrangement, no focus switching
        swap_left = actions.swap_left,
        swap_right = actions.swap_right,
        swap_up = actions.swap_up,
        swap_down = actions.swap_down,
        full_width = actions.full_width,
        cycle_width = actions.cycle_width,
        center_window = actions.center_window,
        toggle_floating = actions.toggle_floating
    }
    
    return paperWMActions
end

local function paperWMAction(actionName)
    return function()
        local actions = loadPaperWM()
        if actions[actionName] then
            actions[actionName]()
        end
    end
end

-- ============================================================================
-- MINIMAL HOTKEYS (Non-conflicting with AltTab)
-- ============================================================================

-- Use Cmd+Ctrl for window arrangement (different from AltTab's modifiers)
local mods = {"cmd", "ctrl"}

-- Window arrangement only (no focus switching - let AltTab handle that)
hs.hotkey.bind(mods, "left", paperWMAction("swap_left"))
hs.hotkey.bind(mods, "right", paperWMAction("swap_right"))
hs.hotkey.bind(mods, "up", paperWMAction("swap_up"))
hs.hotkey.bind(mods, "down", paperWMAction("swap_down"))

-- Window sizing
hs.hotkey.bind(mods, "f", paperWMAction("full_width"))
hs.hotkey.bind(mods, "c", paperWMAction("center_window"))
hs.hotkey.bind(mods, "r", paperWMAction("cycle_width"))
hs.hotkey.bind(mods, "escape", paperWMAction("toggle_floating"))

-- ============================================================================
-- QUICK WINDOW OPERATIONS (Complement AltTab)
-- ============================================================================

-- Minimize/Close operations that AltTab doesn't handle well
hs.hotkey.bind({"cmd", "alt"}, "m", function()
    local win = hs.window.focusedWindow()
    if win then win:minimize() end
end)

hs.hotkey.bind({"cmd", "alt"}, "q", function()
    local win = hs.window.focusedWindow()
    if win then win:close() end
end)

-- Quick maximize (different from PaperWM's full_width)
hs.hotkey.bind({"cmd", "alt"}, "return", function()
    local win = hs.window.focusedWindow()
    if win then win:maximize(0) end
end)

-- ============================================================================
-- MEMORY MANAGEMENT
-- ============================================================================

-- Light garbage collection every 10 minutes
hs.timer.doEvery(600, function()
    if collectgarbage("count") > 20000 then
        collectgarbage("collect")
    end
end)

-- ============================================================================
-- RELOAD (Manual only)
-- ============================================================================

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "r", function()
    collectgarbage("collect")
    hs.reload()
end)

-- ============================================================================
-- STARTUP
-- ============================================================================

-- Minimal startup
hs.timer.doAfter(0.1, function()
    collectgarbage("collect")
    
    -- Optional: Ensure AltTab is running
    if not hs.application.get("AltTab") then
        hs.application.open("AltTab")
    end
end)

-- Silent load
print("Hammerspoon loaded (AltTab compatible mode)")