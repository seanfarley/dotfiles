local ipc = require 'hs.ipc'

local emacs = require "emacs"
local keybindings = require "keybindings"
local reload = require 'utils/reload'

-- 'info' is a bit too verbose
hs.logger.setGlobalLogLevel('warning')

-- make sure command-line utility is enabled and installed
ipc.cliInstall()

reload.init()
keybindings.init()

hs.alert.show("Config loaded")
