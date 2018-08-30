function reloadConfig(files)
    doReload = false
    for _,file in pairs(files) do
        if file:sub(-4) == ".lua" then
            doReload = true
        end
    end
    if doReload then
        hs.reload()
    end
end
local myWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/init.lua", reloadConfig):start()
hs.alert.show("Config loaded")

-- left half window
hs.hotkey.bind({"cmd", "shift", "ctrl"}, "Left", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = max.w / 2
  f.h = max.h
  win:setFrame(f)
end)

-- right half window
hs.hotkey.bind({"cmd", "shift", "ctrl"}, "Right", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x + (max.w / 2)
  f.y = max.y
  f.w = max.w / 2
  f.h = max.h
  win:setFrame(f)
end)

-- top half window
hs.hotkey.bind({"cmd", "shift", "ctrl"}, "Up", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = max.w
  f.h = max.h / 2
  win:setFrame(f)
end)

-- bottom half window
hs.hotkey.bind({"cmd", "shift", "ctrl"}, "Down", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y + (max.h / 2)
  f.w = max.w
  f.h = max.h / 2
  win:setFrame(f)
end)

-- full screen window
hs.hotkey.bind({"cmd", "shift", "ctrl"}, "/", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = max.w
  f.h = max.h
  win:setFrame(f)
end)


hs.hotkey.bind({"cmd", "ctrl"}, "S", function()
      hs.application.launchOrFocus("Firefox")
end)

hs.hotkey.bind({"cmd", "ctrl"}, "C", function()
      hs.application.launchOrFocus("Firefox")
end)

hs.hotkey.bind({"cmd", "ctrl"}, "E", function()
      hs.application.launchOrFocus("/Applications/Emacs.app")
end)

hs.hotkey.bind({"cmd", "ctrl"}, "T", function()
      hs.application.launchOrFocus("iTerm")
end)

hs.hotkey.bind({"cmd", "ctrl"}, "L", function()
      hs.application.launchOrFocus("Calendar")
end)

hs.hotkey.bind({"cmd", "ctrl"}, "I", function()
      hs.application.launchOrFocus("iTunes")
end)

-- map emacs keybindings in everything but the emacs app
local esc = hs.hotkey.new({"ctrl"}, "g", function()
      hs.eventtap.keyStroke({}, "escape")
end)

function applicationWatcher(appName, eventType, appObject)
   if (eventType == hs.application.watcher.activated) then
      if (appName == "Emacs") then
         esc:disable()
      else
         esc:enable()
      end
   end
end
appWatcher = hs.application.watcher.new(applicationWatcher)
appWatcher:start()
