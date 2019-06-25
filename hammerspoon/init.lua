local ipc = require 'hs.ipc'

local emacs = require "emacs"
local keybindings = require "keybindings"
local reload = require 'utils/reload'

-- make sure command-line utility is enabled and installed
ipc.cliInstall()

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
