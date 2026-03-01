-- FINAL OPTIMIZED HAMMERSPOON CONFIG
-- All performance optimizations applied, no external tools needed

-- ============================================================================
-- CORE PERFORMANCE SETTINGS
-- ============================================================================

-- Disable ALL animations and transitions
hs.window.animationDuration = 0
hs.alert.defaultStyle.animationDuration = 0

-- Disable all logging for maximum performance
hs.window.filter.setLogLevel(0)
hs.hotkey.setLogLevel(0)
hs.timer.setLogLevel(0)
hs.logger.setGlobalLogLevel(0)

-- Hide menu bar icon (saves resources)
hs.menuIcon(false)

-- ============================================================================
-- GARBAGE COLLECTION OPTIMIZATION
-- ============================================================================

-- More aggressive garbage collection
collectgarbage("setpause", 100)
collectgarbage("setstepmul", 200)

-- ============================================================================
-- LAZY LOADING PAPERWM
-- ============================================================================

local paperWMLoaded = false
local paperWMActions = {}

local function loadPaperWM()
    if paperWMLoaded then return paperWMActions end
    paperWMLoaded = true
    
    PaperWM = hs.loadSpoon("PaperWM")
    
    -- Minimal configuration for speed
    PaperWM.window_gap = 8
    PaperWM.center_mouse = false
    
    -- Aggressive window filtering - exclude all system apps
    PaperWM.window_filter = hs.window.filter.new()
    local excludeApps = {
        'Finder', 'System Preferences', 'System Settings',
        'Activity Monitor', 'Console', 'Archive Utility',
        'Calculator', 'Dictionary', 'Font Book', 'Screenshot',
        'Photo Booth', 'QuickTime Player', 'Stickies',
        'ColorSync Utility', 'Migration Assistant',
        'AirPort Utility', 'Bluetooth File Exchange',
        'Keychain Access', 'Terminal', 'Disk Utility',
        'System Information', 'Automator', 'Script Editor',
        'Digital Color Meter', 'Grapher', 'VoiceOver Utility',
        'Audio MIDI Setup', 'Wireless Diagnostics'
    }
    for _, app in ipairs(excludeApps) do
        PaperWM.window_filter:setAppFilter(app, false)
    end
    
    -- Only tile on primary screen for performance
    local screens = hs.screen.allScreens()
    if #screens > 1 then
        PaperWM.screen_filter = {
            [hs.screen.primaryScreen():id()] = true
        }
    end
    
    PaperWM:start()
    
    -- Cache actions for direct access
    local actions = PaperWM.actions.actions()
    paperWMActions = {
        focus_left = actions.focus_left,
        focus_right = actions.focus_right,
        focus_up = actions.focus_up,
        focus_down = actions.focus_down,
        swap_left = actions.swap_left,
        swap_right = actions.swap_right,
        full_width = actions.full_width,
        cycle_width = actions.cycle_width,
        toggle_floating = actions.toggle_floating
    }
    
    return paperWMActions
end

-- Create wrapper function for lazy loading
local function paperWMAction(actionName)
    return function()
        local actions = loadPaperWM()
        if actions[actionName] then
            actions[actionName]()
        end
    end
end

-- ============================================================================
-- OPTIMIZED HOTKEYS - Using simpler modifiers
-- ============================================================================

-- Use Cmd+Alt for primary actions (faster than Hyper key)
local mods = {"cmd", "alt"}
local shiftMods = {"cmd", "alt", "shift"}

-- Window navigation
hs.hotkey.bind(mods, "h", paperWMAction("focus_left"))
hs.hotkey.bind(mods, "l", paperWMAction("focus_right"))
hs.hotkey.bind(mods, "j", paperWMAction("focus_down"))
hs.hotkey.bind(mods, "k", paperWMAction("focus_up"))

-- Window swapping
hs.hotkey.bind(shiftMods, "h", paperWMAction("swap_left"))
hs.hotkey.bind(shiftMods, "l", paperWMAction("swap_right"))

-- Window sizing
hs.hotkey.bind(mods, "f", paperWMAction("full_width"))
hs.hotkey.bind(mods, "r", paperWMAction("cycle_width"))
hs.hotkey.bind(mods, "escape", paperWMAction("toggle_floating"))

-- ============================================================================
-- MEMORY MANAGEMENT
-- ============================================================================

-- Periodic garbage collection (every 5 minutes)
hs.timer.doEvery(300, function()
    local beforeMem = collectgarbage("count")
    collectgarbage("collect")
    
    -- If memory usage is still high, do a full collection
    if collectgarbage("count") > 30000 then  -- 30MB threshold
        collectgarbage("collect")
        collectgarbage("collect")  -- Double collect
    end
end)

-- ============================================================================
-- MINIMAL RELOAD FUNCTIONALITY
-- ============================================================================

-- Manual reload only (no file watching)
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "r", function()
    -- Clear everything before reload
    paperWMLoaded = false
    paperWMActions = {}
    collectgarbage("collect")
    hs.reload()
end)

-- ============================================================================
-- STARTUP OPTIMIZATION
-- ============================================================================

-- Defer non-critical initialization
hs.timer.doAfter(0.1, function()
    -- Only load IPC if the socket exists
    if hs.fs.attributes(os.getenv("HOME") .. "/.hammerspoon/ipc") then
        require("hs.ipc")
    end
    
    -- Force garbage collection after startup
    collectgarbage("collect")
end)

-- No startup notification (saves time)