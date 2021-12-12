local ipc = require 'hs.ipc'

local emacs = require "emacs"
local keybindings = require "keybindings"
local reload = require 'utils/reload'
local clipboard = require 'clipboard'

-- most of this code is taken from https://github.com/j-martin/dotfiles
-- 'info' is a bit too verbose
hs.logger.setGlobalLogLevel('warning')

-- make sure command-line utility is enabled and installed
ipc.cliInstall()

reload.init()
keybindings.init()
clipboard.init()

hs.alert.show("Config loaded")
