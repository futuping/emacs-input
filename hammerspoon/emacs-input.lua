-- emacs-input.lua - Hammerspoon integration for emacs-input
-- Provides system integration for getting app info, selection, and pasting

local emacs_input = {}

-- Internal state for focus monitoring
local focus_watcher = nil
local auto_trigger_timer = nil
local last_focused_element = nil

-- Configuration
local config = {
    -- Global hotkey to trigger emacs-input (default: Cmd+E)
    hotkey_mods = {"cmd"},
    hotkey_key = "e",
    -- Emacsclient command
    emacsclient = "/run/current-system/sw/bin/emacsclient",
    -- Auto-trigger on input focus
    auto_trigger_on_focus = false,
    -- Delay before auto-triggering (in seconds)
    auto_trigger_delay = 0.5,
    -- Applications to exclude from auto-trigger
    excluded_apps = {
        "Emacs",
        "Terminal",
        "iTerm2",
        "Xcode",
        "Visual Studio Code",
        "IntelliJ IDEA",
        "PyCharm",
        "WebStorm"
    }
}

-- Get information about the currently focused application
function emacs_input.getAppInfo()
    local app = hs.application.frontmostApplication()
    local window = app:focusedWindow()
    
    if not app or not window then
        print("nil")
        return
    end
    
    local info = {
        name = app:name(),
        bundleID = app:bundleID(),
        title = window:title() or "",
        frame = window:frame()
    }
    
    -- Print as Lisp-readable format
    print(string.format('(:name "%s" :bundle-id "%s" :title "%s" :frame (%d %d %d %d))',
        info.name:gsub('"', '\\"'),
        info.bundleID or "",
        info.title:gsub('"', '\\"'),
        math.floor(info.frame.x),
        math.floor(info.frame.y),
        math.floor(info.frame.w),
        math.floor(info.frame.h)))
end

-- Get currently selected text
function emacs_input.getSelection()
    -- Store current clipboard
    local originalClipboard = hs.pasteboard.getContents()
    
    -- Copy selection to clipboard
    hs.eventtap.keyStroke({"cmd"}, "c")
    
    -- Small delay to ensure clipboard is updated
    hs.timer.usleep(50000) -- 50ms
    
    local selection = hs.pasteboard.getContents() or ""
    
    -- Restore original clipboard if selection was empty or same
    if selection == "" or selection == originalClipboard then
        selection = ""
    else
        -- Restore original clipboard after getting selection
        hs.timer.doAfter(0.1, function()
            if originalClipboard then
                hs.pasteboard.setContents(originalClipboard)
            end
        end)
    end
    
    print(selection)
end

-- Paste content back to the original application
function emacs_input.pasteContent(content)
    if not content or content == "" then
        return
    end
    
    -- Set clipboard content
    hs.pasteboard.setContents(content)
    
    -- Small delay then paste
    hs.timer.doAfter(0.05, function()
        hs.eventtap.keyStroke({"cmd"}, "v")
    end)
end

-- Focus back to the original application
function emacs_input.focusOriginalApp(bundleID)
    if bundleID then
        local app = hs.application.get(bundleID)
        if app then
            app:activate()
        end
    end
end

-- Check if current app should be excluded from auto-trigger
function emacs_input.isAppExcluded(appName)
    if not appName then return true end

    for _, excludedApp in ipairs(config.excluded_apps) do
        if string.find(appName, excludedApp, 1, true) then
            return true
        end
    end
    return false
end

-- Check if the focused element is likely an input field
function emacs_input.isInputElement()
    local app = hs.application.frontmostApplication()
    if not app then return false end

    -- Get accessibility element under mouse cursor or focused element
    local axApp = hs.axuielement.applicationElement(app)
    if not axApp then return false end

    local focusedElement = axApp:attributeValue("AXFocusedUIElement")
    if not focusedElement then return false end

    -- Check if the focused element is a text field or text area
    local role = focusedElement:attributeValue("AXRole")
    local subrole = focusedElement:attributeValue("AXSubrole")

    -- Common input field roles
    local inputRoles = {
        "AXTextField",
        "AXTextArea",
        "AXComboBox",
        "AXSearchField"
    }

    local inputSubroles = {
        "AXSearchField",
        "AXSecureTextField"
    }

    -- Check role
    for _, inputRole in ipairs(inputRoles) do
        if role == inputRole then
            return true
        end
    end

    -- Check subrole
    if subrole then
        for _, inputSubrole in ipairs(inputSubroles) do
            if subrole == inputSubrole then
                return true
            end
        end
    end

    return false
end

-- Handle focus change for auto-trigger
function emacs_input.onFocusChanged()
    if not config.auto_trigger_on_focus then
        return
    end

    -- Cancel any pending auto-trigger
    if auto_trigger_timer then
        auto_trigger_timer:stop()
        auto_trigger_timer = nil
    end

    local app = hs.application.frontmostApplication()
    if not app then return end

    local appName = app:name()

    -- Skip if app is excluded
    if emacs_input.isAppExcluded(appName) then
        return
    end

    -- Check if focused element is an input field
    if emacs_input.isInputElement() then
        -- Set timer to auto-trigger after delay
        auto_trigger_timer = hs.timer.doAfter(config.auto_trigger_delay, function()
            emacs_input.trigger()
            auto_trigger_timer = nil
        end)
    end
end

-- Trigger emacs-input
function emacs_input.trigger()
    -- Call emacsclient to trigger fast emacs-input with GUI client
    -- Use -c to create client frame, then emacs-input-fast will use that frame
    local task = hs.task.new(config.emacsclient, function(exitCode, stdOut, stdErr)
        if exitCode ~= 0 then
            hs.alert.show("Failed to launch emacs-input: " .. (stdErr or "unknown error"))
        end
    end, {"-e", "(emacs-input-fast)"})

    task:start()
end

-- Enable/disable auto-trigger on focus
function emacs_input.setAutoTrigger(enabled)
    config.auto_trigger_on_focus = enabled

    if enabled then
        -- Start focus monitoring
        if not focus_watcher then
            focus_watcher = hs.application.watcher.new(function(appName, eventType, appObject)
                if eventType == hs.application.watcher.activated then
                    -- Small delay to let the app fully activate
                    hs.timer.doAfter(0.1, emacs_input.onFocusChanged)
                end
            end)
            focus_watcher:start()
        end

        -- Also monitor accessibility events for more precise input field detection
        if not emacs_input.axObserver then
            emacs_input.axObserver = hs.axuielement.observer.new(hs.application.frontmostApplication():pid())
            emacs_input.axObserver:addWatcher(hs.axuielement.observer.notifications.focusedUIElementChanged,
                function(element, notification, observer)
                    hs.timer.doAfter(0.1, emacs_input.onFocusChanged)
                end)
            emacs_input.axObserver:start()
        end

        hs.alert.show("emacs-input auto-trigger enabled")
    else
        -- Stop focus monitoring
        if focus_watcher then
            focus_watcher:stop()
            focus_watcher = nil
        end

        if emacs_input.axObserver then
            emacs_input.axObserver:stop()
            emacs_input.axObserver = nil
        end

        -- Cancel any pending auto-trigger
        if auto_trigger_timer then
            auto_trigger_timer:stop()
            auto_trigger_timer = nil
        end

        hs.alert.show("emacs-input auto-trigger disabled")
    end
end

-- Setup global hotkey and optional auto-trigger
function emacs_input.setup()
    -- Bind global hotkey
    hs.hotkey.bind(config.hotkey_mods, config.hotkey_key, emacs_input.trigger)

    -- Setup auto-trigger if enabled
    if config.auto_trigger_on_focus then
        emacs_input.setAutoTrigger(true)
    end

    hs.alert.show("emacs-input loaded (⌘E)")
end

-- Cleanup
function emacs_input.cleanup()
    if emacs_input.hotkeyObj then
        emacs_input.hotkeyObj:delete()
    end

    -- Stop focus monitoring
    if focus_watcher then
        focus_watcher:stop()
        focus_watcher = nil
    end

    if emacs_input.axObserver then
        emacs_input.axObserver:stop()
        emacs_input.axObserver = nil
    end

    -- Cancel any pending auto-trigger
    if auto_trigger_timer then
        auto_trigger_timer:stop()
        auto_trigger_timer = nil
    end
end

-- Expose config for external modification
emacs_input.config = config

return emacs_input
