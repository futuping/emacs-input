-- emacs-input.lua - Hammerspoon integration for emacs-input
-- Provides system integration for getting app info and pasting

local emacs_input = {}

-- Configuration
local config = {
    -- Global hotkey to trigger emacs-input (default: Cmd+E)
    hotkey_mods = {"cmd"},
    hotkey_key = "e",
    -- Emacsclient command
    emacsclient = "/run/current-system/sw/bin/emacsclient"
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

-- Trigger emacs-input
function emacs_input.trigger()
    -- Call emacsclient to trigger emacs-input
    local task = hs.task.new(config.emacsclient, function(exitCode, stdOut, stdErr)
        if exitCode ~= 0 then
            hs.alert.show("Failed to launch emacs-input: " .. (stdErr or "unknown error"))
        end
    end, {"-e", "(emacs-input)"})

    task:start()
end

-- Setup global hotkey
function emacs_input.setup()
    -- Bind global hotkey
    hs.hotkey.bind(config.hotkey_mods, config.hotkey_key, emacs_input.trigger)

    hs.alert.show("emacs-input loaded (⌘E)")
end

-- Cleanup
function emacs_input.cleanup()
    if emacs_input.hotkeyObj then
        emacs_input.hotkeyObj:delete()
    end
end

return emacs_input
