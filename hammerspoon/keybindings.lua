local apps = require 'apps'
local audio = require 'audio'
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

local mod = {}

-----------------------------
-- binding per application --
-----------------------------

-- Default is hyper

local bindings = {
  { name = keybinder.globalBindings,
    bindings = {
      { modifiers = cmdCtrl, key = 's', name = 'Safari' },
      { modifiers = cmdCtrl, key = 'e', name = 'Emacs' },
      { modifiers = cmdCtrl, key = 't', name = 'iTerm' },
      { modifiers = cmdCtrl, key = 'l', name = 'Calendar' },
      { modifiers = cmdCtrl, key = 'c', name = 'Slack' },

      { key = 'up', fn = function () hs.eventtap.keyStroke({}, "pageup") end, desc = 'Page Up' },
      { key = 'down', fn = function () hs.eventtap.keyStroke({}, "pagedown") end, desc = 'Page Down' },
      { key = 'left', fn = function () hs.eventtap.keyStroke({}, "home") end, desc = 'Page Top' },
      { key = 'right', fn = function () hs.eventtap.keyStroke({}, "end") end, desc = 'Page Bottom' },

      -- map emacs keybindings in everything but the emacs app
      { modifiers = {'ctrl'}, key = 'g', filter = { 'Emacs' }, fn = function() hs.eventtap.keyStroke({}, "escape") end },

      { key = 'n', fn = apps.openNotification, desc = 'Notification - Open' },
      { key = 'n', fn = apps.openNotificationAction, shift = true, desc = 'Notification - Action' },

      { key = 'h', pos = { { 0.0, 0.0, 0.5, 1.0}, { 0.0, 0.0, 0.5, 1.0} }, desc = 'Window - Left 50%', shift = true },
      { key = 'j', pos = { { 0.0, 0.5, 1.0, 0.5}, { 0.0, 0.5, 1.0, 0.5} }, desc = 'Window - Top 50%', shift = true },
      { key = 'k', pos = { { 0.0, 0.0, 1.0, 0.5}, { 0.0, 0.0, 1.0, 0.5} }, desc = 'Window - Bottom 50%', shift = true },
      { key = 'l', pos = { { 0.5, 0.0, 0.5, 1.0}, { 0.5, 0.0, 0.5, 1.0} }, desc = 'Window - Right 50%', shift = true },
      { key = ';', pos = { { 0.0, 0.0, 1.0, 1.0}, { 0.0, 0.0, 1.0, 1.0} }, desc = 'Window - Fullscreen', shift = true },
    }
  },
  -- {
  --   name = chrome.name,
  --   bindings = {
  --     { modifiers = cmdCtrl, key = 'u', fn = chrome.slackReactionEmoji('thup'), desc = 'Thumbs up' },
  --     { modifiers = cmdCtrl, key = 's', fn = chrome.slackReactionEmoji('slighsm'), desc = 'Smiling Face' },
  --     { modifiers = cmdCtrl, key = 'e', fn = chrome.slackReactionEmoji('heart'), desc = 'Heart' },
  --   }
  -- }
}

----------------
-- hyper mode --
----------------

local hyperModeBindings = {
  { key = 'b', fn = screen.setBrightness(0.8), desc = 'Set brightness to 80%.' },
  { key = 'e', fn = mounts.unmountAll, desc = 'Unmount all volumes' },
  { key = 'h', fn = audio.current, desc = 'Current song' },
  { key = 'i', fn = audio.changeVolume(-5), desc = 'Decrease the volume by 5%' },
  { key = 'j', fn = audio.next, desc = 'Next song' },
  { key = 'k', fn = audio.previous, desc = 'Previous song' },
  { key = 'o', fn = audio.setVolume(15), desc = 'Default volume level' },
  { key = 'p', fn = audio.setVolume(30), desc = 'High volume level' },
  { key = 'r', fn = reload.reload, desc = 'Reloading configuration ...' },
  { key = 'space', fn = audio.playpause, exitMode = true, desc = 'Pause or resume' },
  { key = 'u', fn = audio.changeVolume(5), desc = 'Increase the volume by 5%' },
  { key = 'm', fn = audio.changeVolume(-100), desc = 'Mute'},
}

function mod.init()
  keybinder.init(bindings)
  mode.create(hyper, 'space', 'Hyper', hyperModeBindings)
end

return mod
