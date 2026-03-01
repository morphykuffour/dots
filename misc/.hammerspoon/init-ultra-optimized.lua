-- ULTRA-OPTIMIZED HAMMERSPOON CONFIG
-- Maximum performance with intelligent lazy loading

-- ============================================================================
-- CORE PERFORMANCE SETTINGS
-- ============================================================================

-- Disable ALL animations and transitions
hs.window.animationDuration = 0
hs.alert.defaultStyle.animationDuration = 0

-- Reduce Hammerspoon's own event processing overhead
hs.window.filter.setLogLevel(0)  -- Disable window filter logging
hs.hotkey.setLogLevel(0)         -- Disable hotkey logging
hs.timer.setLogLevel(0)          -- Disable timer logging

-- Disable Hammerspoon's menu bar icon (saves resources)
-- hs.menuIcon(false)  -- Uncomment to hide menu bar icon

-- ============================================================================
-- LAZY LOADING SYSTEM
-- ============================================================================

local loadedModules = {}

local function lazyLoad(moduleName, setupFn)
    return function()
        if not loadedModules[moduleName] then
            loadedModules[moduleName] = true
            setupFn()
        end
    end
end

-- ============================================================================
-- OPTIMIZED PAPERWM (Load on first use)
-- ============================================================================

local paperWMLoaded = false
local paperWMActions = {}

local function loadPaperWM()
    if paperWMLoaded then return end
    paperWMLoaded = true
    
    PaperWM = hs.loadSpoon("PaperWM")
    
    -- Minimal configuration
    PaperWM.window_gap = 8
    PaperWM.center_mouse = false
    
    -- Aggressive window filtering
    PaperWM.window_filter = hs.window.filter.new()
    local excludeApps = {
        'Finder', 'System Preferences', 'System Settings',
        'Activity Monitor', 'Console', 'Archive Utility',
        'Calculator', 'Dictionary', 'Font Book', 'Screenshot',
        'Photo Booth', 'QuickTime Player', 'Stickies',
        'ColorSync Utility', 'Migration Assistant',
        'AirPort Utility', 'Bluetooth File Exchange'
    }
    for _, app in ipairs(excludeApps) do
        PaperWM.window_filter:setAppFilter(app, false)
    end
    
    -- Only tile on specific screens if multiple monitors
    local screens = hs.screen.allScreens()
    if #screens > 1 then
        -- Only tile on primary screen for performance
        PaperWM.screen_filter = {
            [hs.screen.primaryScreen():id()] = true
        }
    end
    
    PaperWM:start()
    
    -- Cache actions for direct access (faster than going through PaperWM)
    local actions = PaperWM.actions.actions()
    paperWMActions = {
        focus_left = actions.focus_left,
        focus_right = actions.focus_right,
        focus_up = actions.focus_up,
        focus_down = actions.focus_down,
        swap_left = actions.swap_left,
        swap_right = actions.swap_right,
        full_width = actions.full_width,
        cycle_width = actions.cycle_width
    }
end

-- Lazy-load PaperWM on first window operation
local function paperWMAction(actionName)
    return function()
        if not paperWMLoaded then loadPaperWM() end
        if paperWMActions[actionName] then
            paperWMActions[actionName]()
        end
    end
end

-- ============================================================================
-- OPTIMIZED HOTKEYS
-- ============================================================================

-- Use simpler modifiers for frequently used commands
local mods = {"cmd", "alt"}
local shiftMods = {"cmd", "alt", "shift"}

-- Window navigation (most used, simplest modifiers)
hs.hotkey.bind(mods, "h", paperWMAction("focus_left"))
hs.hotkey.bind(mods, "l", paperWMAction("focus_right"))
hs.hotkey.bind(mods, "j", paperWMAction("focus_down"))
hs.hotkey.bind(mods, "k", paperWMAction("focus_up"))

-- Window arrangement (less frequent, add shift)
hs.hotkey.bind(shiftMods, "h", paperWMAction("swap_left"))
hs.hotkey.bind(shiftMods, "l", paperWMAction("swap_right"))
hs.hotkey.bind(mods, "f", paperWMAction("full_width"))
hs.hotkey.bind(mods, "r", paperWMAction("cycle_width"))

-- ============================================================================
-- FAST APPLICATION SWITCHING
-- ============================================================================

-- Cache frequently used applications
local appCache = {}
local appCacheTimeout = 60  -- Refresh cache every 60 seconds

local function getApp(appName)
    local now = hs.timer.secondsSinceEpoch()
    if not appCache[appName] or (now - appCache[appName].time) > appCacheTimeout then
        appCache[appName] = {
            app = hs.application.find(appName),
            time = now
        }
    end
    return appCache[appName].app
end

-- Fast app switching with single modifier
local function fastAppSwitch(appName)
    return function()
        local app = getApp(appName)
        if app then
            app:activate()
        else
            -- Fallback to launch if not running
            hs.application.launchOrFocus(appName)
        end
    end
end

-- Bind fast app switching (customize these)
hs.hotkey.bind({"cmd"}, "1", fastAppSwitch("Brave Browser"))
hs.hotkey.bind({"cmd"}, "2", fastAppSwitch("Terminal"))
hs.hotkey.bind({"cmd"}, "3", fastAppSwitch("Visual Studio Code"))
-- Add more as needed

-- ============================================================================
-- CONDITIONAL MODULE LOADING
-- ============================================================================

-- Only load expensive modules when explicitly requested
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "p", lazyLoad("pip", function()
    -- PIP watcher code here (only loads when activated)
    print("Loading PIP module...")
    -- ... PIP code ...
end))

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "b", lazyLoad("borders", function()
    -- Window border code here (only loads when activated)
    print("Loading window borders...")
    -- ... border code ...
end))

-- ============================================================================
-- PERFORMANCE MONITORING (Optional)
-- ============================================================================

local perfMon = false  -- Set to true to enable

if perfMon then
    local lastCheck = hs.timer.absoluteTime()
    
    hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "p", function()
        local now = hs.timer.absoluteTime()
        local delta = (now - lastCheck) / 1000000  -- Convert to milliseconds
        lastCheck = now
        
        local memUsage = collectgarbage("count")
        hs.alert.show(string.format("Response time: %.2fms\nMem: %.2f KB", delta, memUsage))
    end)
end

-- ============================================================================
-- GARBAGE COLLECTION OPTIMIZATION
-- ============================================================================

-- More aggressive garbage collection
collectgarbage("setpause", 100)
collectgarbage("setstepmul", 200)

-- Manual GC after setup
hs.timer.doAfter(1, function()
    collectgarbage("collect")
end)

-- ============================================================================
-- MINIMAL RELOAD
-- ============================================================================

-- Only reload what's necessary
local function smartReload()
    -- Clear caches
    appCache = {}
    loadedModules = {}
    
    -- Force garbage collection
    collectgarbage("collect")
    
    -- Reload
    hs.reload()
end

hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "r", smartReload)

-- ============================================================================
-- STARTUP
-- ============================================================================

-- Defer non-critical initialization
hs.timer.doAfter(0.1, function()
    -- Load IPC only if needed for hammerspoon CLI
    if hs.fs.attributes(os.getenv("HOME") .. "/.hammerspoon/ipc") then
        require("hs.ipc")
    end
    
    -- Collect garbage after startup
    collectgarbage("collect")
end)

-- Silent, fast notification
if hs.settings.get("hammerspoonStartupNotification") ~= false then
    print("Hammerspoon ready")
end