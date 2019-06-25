local ipc = require 'hs.ipc'

local emacs = require "emacs"
local keybindings = require "keybindings"
local reload = require 'utils/reload'

-- make sure command-line utility is enabled and installed
ipc.cliInstall()

reload.init()
keybindings.init()

hs.alert.show("Config loaded")
