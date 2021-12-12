local apps = require 'apps'
local keybinder = require 'keybinder'
local emacs = require 'emacs'
local grid = require 'hs.grid'
local mode = require 'mode'
local mounts = require 'mounts'
local screen = require 'screen'
local selection = require 'selection'
local windows = require 'windows'
local reload = require 'utils/reload'

local cmd = keybinder.cmd
local cmdCtrl = { 'cmd', 'ctrl' }
local hyper = keybinder.hyper

local mod = {
  hyper = hyper,
}

-----------------------------
-- binding per application --
-----------------------------

-- Default is hyper

local bindings = {
  { name = keybinder.globalBindings,
    bindings = {
      { modifiers = cmdCtrl, key = 's', name = 'Safari' },
      { modifiers = cmdCtrl, key = 'l', name = 'Calendar' },
      { modifiers = cmdCtrl, key = 'e', fn = emacs.focus, desc = 'Emacs' },
      { modifiers = cmdCtrl, key = 'w', fn = emacs.switchWorkspace, desc = 'Emacs' },
      { modifiers = cmdCtrl, key = 't', fn = emacs.vterm, desc = 'vterm' },
      { modifiers = cmdCtrl, key = 'c', name = 'Element' },
      { modifiers = cmdCtrl, key = 'i', name = 'Music' },
      { modifiers = cmdCtrl, key = 'm', fn = emacs.mu4e, desc = 'mu4e' },
      { modifiers = cmdCtrl, key = 'r', fn = emacs.orgroam, desc = 'org-roam' },

      -- { modifiers = cmd, key = 'space', fn = emacs.mu4e, desc = 'mu4e' },

      { key = 'up', fn = function () hs.eventtap.keyStroke({}, "pageup") end, desc = 'Page Up' },
      { key = 'down', fn = function () hs.eventtap.keyStroke({}, "pagedown") end, desc = 'Page Down' },
      { key = 'left', fn = function () hs.eventtap.keyStroke({}, "home") end, desc = 'Page Top' },
      { key = 'right', fn = function () hs.eventtap.keyStroke({}, "end") end, desc = 'Page Bottom' },

      -- map emacs keybindings in everything but the emacs app
      { modifiers = {'ctrl'}, key = 'g', fn = function() hs.eventtap.keyStroke({}, "escape") end, desc = 'C-g is ESC'},

      { key = 'n', fn = apps.openNotification, desc = 'Notification - Open' },
      { key = 'n', fn = apps.openNotificationAction, shift = true, desc = 'Notification - Action' },

      { key = 'h', pos = { { 0.0, 0.0, 0.5, 1.0}, { 0.0, 0.0, 0.5, 1.0} }, desc = 'Window - Left 50%' },
      { key = 'j', pos = { { 0.0, 0.5, 1.0, 0.5}, { 0.0, 0.5, 1.0, 0.5} }, desc = 'Window - Top 50%' },
      { key = 'k', pos = { { 0.0, 0.0, 1.0, 0.5}, { 0.0, 0.0, 1.0, 0.5} }, desc = 'Window - Bottom 50%' },
      { key = 'l', pos = { { 0.5, 0.0, 0.5, 1.0}, { 0.5, 0.0, 0.5, 1.0} }, desc = 'Window - Right 50%' },
      { key = ';', pos = { { 0.0, 0.0, 1.0, 1.0}, { 0.0, 0.0, 1.0, 1.0} }, desc = 'Window - Fullscreen' },

    }
  },
}

----------------
-- hyper mode --
----------------

local hyperModeBindings = {
  -- { key = 'b', fn = screen.setBrightness(0.8), desc = 'Set brightness to 80%.' },
  { key = 'u', fn = mounts.unmountAll, desc = 'Unmount all volumes' },
  { key = 'r', fn = reload.reload, desc = 'Reload hammerspoon' },
  { key = 'c', fn = hs.toggleConsole, desc = 'Console' },
  { key = 'e', fn = emacs.everywhere, desc = 'Emacs Everywhere'},
}

----------------
-- org mode --
----------------

local orgModeBindings = {
  { key = 'r', fn = function () emacs.capture('roam-ref', 'r') end, desc = 'Roam Capture' },
  { key = 'n', fn = emacs.capture, desc = 'Capture' },
  { key = 'a', fn = emacs.agenda, desc = 'Agenda' },

  -- { modifiers = { 'ctrl' }, key = 'c', fn = function () hs.alert.show("Testing C-c") end, desc = 'Testin C-c', filter = { 'Emacs' } },

}

local ctrlCBindings = {
  { key = 'n',
    fn = function ()
      local notes = mode.create({}, 'n', '+notes', orgModeBindings, nil, true)
      notes:enter()
    end,
    desc = '+notes' },

}

function mod.init()
  keybinder.init(bindings)
  mode.create(hyper, 'space', 'Hyper', hyperModeBindings)
  mode.create({ 'ctrl' }, 'c', 'C-c', ctrlCBindings)
end

return mod
