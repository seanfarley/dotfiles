local ipc = require 'hs.ipc'

local emacs = require "emacs"
local keybindings = require "keybindings"
local reload = require 'utils/reload'

-- make sure command-line utility is enabled and installed
ipc.cliInstall()

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

-- map emacs keybindings in everything but the emacs app
local esc = hs.hotkey.new({"ctrl"}, "g", function()
      hs.eventtap.keyStroke({}, "escape")
end)

-- after much research, applicationWatcher is the best way to achieve what we
-- want for ctrl-g with emacs
local function applicationWatcher(appName, eventType, appObject)
   if (eventType == hs.application.watcher.activated) then
      if (appName == "Emacs") then
         esc:disable()
      else
         esc:enable()
      end
   end
end
local appWatcher = hs.application.watcher.new(applicationWatcher)
appWatcher:start()

reload.init()
keybindings.init()

hs.alert.show("Config loaded")
