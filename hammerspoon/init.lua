local ipc = require 'hs.ipc'

local emacs = require "emacs"
local keybindings = require "keybindings"
local reload = require 'utils/reload'
local clipboard = require 'clipboard'

-- 'info' is a bit too verbose
hs.logger.setGlobalLogLevel('warning')

-- make sure command-line utility is enabled and installed
ipc.cliInstall()

reload.init()
keybindings.init()
clipboard.init()

hs.loadSpoon("Seal")

spoon.Seal:loadPlugins({"apps", "screencapture", "calc", "useractions", "pasteboard"})

spoon.Seal.plugins.apps.appSearchPaths = {
   "/Applications",
   "~/Applications",
   "/System/Library/PreferencePanes",
}

spoon.Seal:refreshAllCommands()
spoon.Seal:bindHotkeys({show = {{"cmd"}, "space"}})
spoon.Seal.plugins.pasteboard.historySize = 4000

spoon.Seal:start()

-- since seal_apps starts the background spotlight work immediately; wait a bit
-- and then force a restart since we modified the app search path
hs.timer.doAfter(0.5, function () spoon.Seal.plugins.apps:restart() end)

hs.alert.show("Config loaded")
