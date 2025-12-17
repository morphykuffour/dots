-- Auto-position Picture-in-Picture windows
-- Customize size and position here
local PIP_WIDTH = 640
local PIP_HEIGHT = 360
local PIP_X_OFFSET = 20  -- pixels from right edge
local PIP_Y_OFFSET = 100 -- pixels from bottom

-- Watch for new windows
pipWatcher = hs.window.filter.new(false)
pipWatcher:setAppFilter('Google Chrome', {allowRoles='AXStandardWindow'})
pipWatcher:setAppFilter('Safari', {allowRoles='AXStandardWindow'})
pipWatcher:setAppFilter('Firefox', {allowRoles='AXStandardWindow'})

pipWatcher:subscribe(hs.window.filter.windowCreated, function(window)
    -- Check if this is a PiP window (usually small and has specific title)
    local title = window:title()
    local role = window:role()

    -- PiP windows are usually titled "Picture in Picture" or similar
    if title:match("Picture") or window:isStandard() == false or
       window:subrole() == "AXFloatingWindow" then

        -- Wait a moment for window to fully initialize
        hs.timer.doAfter(0.1, function()
            local screen = window:screen()
            local screenFrame = screen:frame()

            -- Position at bottom-right
            local x = screenFrame.x + screenFrame.w - PIP_WIDTH - PIP_X_OFFSET
            local y = screenFrame.y + screenFrame.h - PIP_HEIGHT - PIP_Y_OFFSET

            window:setFrame({
                x = x,
                y = y,
                w = PIP_WIDTH,
                h = PIP_HEIGHT
            })
        end)
    end
end)

-- ============================================================================
-- PaperWM - Tiling Window Manager
-- ============================================================================

PaperWM = hs.loadSpoon("PaperWM")

-- Configure window gaps
PaperWM.window_gap = 8

-- Configure window ratios for cycling
PaperWM.window_ratios = { 1/3, 1/2, 2/3 }

-- Bind hotkeys
PaperWM:bindHotkeys({
    -- switch to a new focused window in tiled grid
    focus_left  = {{"ctrl", "alt", "cmd"}, "h"},
    focus_right = {{"ctrl", "alt", "cmd"}, "l"},
    focus_up    = {{"ctrl", "alt", "cmd"}, "k"},
    focus_down  = {{"ctrl", "alt", "cmd"}, "j"},

    -- move windows around in tiled grid
    swap_left  = {{"ctrl", "alt", "cmd", "shift"}, "h"},
    swap_right = {{"ctrl", "alt", "cmd", "shift"}, "l"},
    swap_up    = {{"ctrl", "alt", "cmd", "shift"}, "k"},
    swap_down  = {{"ctrl", "alt", "cmd", "shift"}, "j"},

    -- position and resize focused window
    center_window = {{"ctrl", "alt", "cmd"}, "c"},
    full_width    = {{"ctrl", "alt", "cmd"}, "f"},
    cycle_width   = {{"ctrl", "alt", "cmd"}, "r"},
    cycle_height  = {{"ctrl", "alt", "cmd", "shift"}, "r"},

    -- move focused window into / out of the tiling layer
    toggle_floating = {{"ctrl", "alt", "cmd"}, "escape"},

    -- switch to a new Mission Control space
    switch_space_l = {{"ctrl", "alt", "cmd"}, ","},
    switch_space_r = {{"ctrl", "alt", "cmd"}, "."},
    switch_space_1 = {{"ctrl", "alt", "cmd"}, "1"},
    switch_space_2 = {{"ctrl", "alt", "cmd"}, "2"},
    switch_space_3 = {{"ctrl", "alt", "cmd"}, "3"},
    switch_space_4 = {{"ctrl", "alt", "cmd"}, "4"},
    switch_space_5 = {{"ctrl", "alt", "cmd"}, "5"},
    switch_space_6 = {{"ctrl", "alt", "cmd"}, "6"},
    switch_space_7 = {{"ctrl", "alt", "cmd"}, "7"},
    switch_space_8 = {{"ctrl", "alt", "cmd"}, "8"},
    switch_space_9 = {{"ctrl", "alt", "cmd"}, "9"},

    -- move focused window to a new space and tile
    move_window_1 = {{"ctrl", "alt", "cmd", "shift"}, "1"},
    move_window_2 = {{"ctrl", "alt", "cmd", "shift"}, "2"},
    move_window_3 = {{"ctrl", "alt", "cmd", "shift"}, "3"},
    move_window_4 = {{"ctrl", "alt", "cmd", "shift"}, "4"},
    move_window_5 = {{"ctrl", "alt", "cmd", "shift"}, "5"},
    move_window_6 = {{"ctrl", "alt", "cmd", "shift"}, "6"},
    move_window_7 = {{"ctrl", "alt", "cmd", "shift"}, "7"},
    move_window_8 = {{"ctrl", "alt", "cmd", "shift"}, "8"},
    move_window_9 = {{"ctrl", "alt", "cmd", "shift"}, "9"}
})

-- Start PaperWM
PaperWM:start()

-- ============================================================================
-- Reload config on save
-- ============================================================================

hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", function(files)
    for _, file in pairs(files) do
        if file:match("init.lua$") then
            hs.reload()
        end
    end
end):start()

hs.alert.show("Hammerspoon config loaded")
