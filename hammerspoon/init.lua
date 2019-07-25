local ipc = require 'hs.ipc'

local emacs = require "emacs"
local keybindings = require "keybindings"
local reload = require 'utils/reload'
local slack = require 'slack'
local clipboard = require 'clipboard'

-- 'info' is a bit too verbose
hs.logger.setGlobalLogLevel('warning')

-- make sure command-line utility is enabled and installed
ipc.cliInstall()

reload.init()
keybindings.init()
slack.init()
clipboard.init()

hs.alert.show("Config loaded")
