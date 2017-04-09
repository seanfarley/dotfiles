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
      hs.application.launchOrFocus("Google Chrome")
end)

hs.hotkey.bind({"cmd", "ctrl"}, "E", function()
      hs.application.launchOrFocus("EmacsMac")
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

hs.hotkey.bind({"ctrl"}, "b", function()
      hs.eventtap.keyStroke({}, "left")
end)
hs.hotkey.bind({"ctrl"}, "f", function()
      hs.eventtap.keyStroke({}, "right")
end)
hs.hotkey.bind({"ctrl"}, "p", function()
      hs.eventtap.keyStroke({}, "up")
end)
hs.hotkey.bind({"ctrl"}, "n", function()
      hs.eventtap.keyStroke({}, "down")
end)

-- map ctrl-g to esc in chrome (so as not to mess with emacs)
local chrome = hs.window.filter.new{'Google Chrome'}
local esc = hs.hotkey.new({"ctrl"}, "g", function()
      hs.eventtap.keyStroke({}, "escape")
end)

chrome:subscribe(hs.window.filter.windowFocused, function () esc:enable() end)
chrome:subscribe(hs.window.filter.windowUnfocused, function() esc:disable() end)
