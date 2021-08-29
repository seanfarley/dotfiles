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
local clipboard = require 'clipboard'

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
      { modifiers = cmdCtrl, key = 'i', fn = emacs.mpc, desc = 'mpc' },
      { modifiers = cmdCtrl, key = 'm', fn = emacs.mu4e, desc = 'mu4e' },

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

      -- { key = 'h', fn = function() hs.execute("/usr/local/bin/mpc prev") end, desc = 'MPC Prev' },
      -- { key = 'j', fn = audio.changeVolume(-5), desc = 'Decrease volume by 5%' },
      -- { key = 'k', fn = audio.changeVolume(5), desc = 'Increase volume by 5%' },
      -- { key = 'l', fn = function() hs.execute("/usr/local/bin/mpc next") end, desc = 'MPC Next' },
      -- { key = ';', fn = function() hs.execute("/usr/local/bin/mpc toggle") end, desc = 'MPC Play / Pause' },

      { modifiers = hyper, key = 'c', fn = function ()
          local currentApp = hs.window.focusedWindow():application():name()
          if currentApp == "Safari" then
            _, time, _ = hs.osascript.javascript("Application('Safari').doJavaScript('" ..
                                                 "hms = document.getElementsByClassName(\"live-time\")[0].textContent.split(\":\");" ..
                                                 "Number(hms[0]) * 3600 + Number(hms[1]) * 60 + Number(hms[2]);" ..
                                                 "', { in: Application('Safari').windows[0].currentTab })")
            _, vid_id, _ = hs.osascript.javascript("Application('Safari').doJavaScript('" ..
                                                   "document.getElementById(\"smf-video-id\").textContent;" ..
                                                   "', { in: Application('Safari').windows[0].currentTab })")

            if vid_id then
              -- back up a bit
              if time > 15 then
                time = time - 15
              end
              local h = math.floor(time / 3600)
              time = time - h * 3600
              local m = math.floor(time / 60)
              time = time - m * 60
              local hms = h .. "h" .. m .. "m" .. time .. "s"
              -- hs.alert.show(hms)
              emacs.capture(nil, "c", hms .. "," .. vid_id)
            else
              hs.alert.show("WARNING: No video id found! Falling back to regular clipping")
              -- focus on one of the nav menus
              hs.osascript.javascript("Application('Safari').doJavaScript('" ..
                                      "document.querySelector(\"button[data-a-target]\").focus();" ..
                                      "', { in: Application('Safari').windows[0].currentTab })")
              -- press the clip shortcut alt-x
              hs.eventtap.keyStroke({'alt'}, 'x')
              -- now new window has popped up for a clip, so need to increment windows[0] to windows[1]
              hs.osascript.javascript("Application('Safari').doJavaScript('" ..
                                      "document.querySelector(\"textarea[data-a-target]\").focus();" ..
                                      "', { in: Application('Safari').windows[1].currentTab })")
            end
          end
      end, desc = "Capture Twitch clip"}
    }
  },
}

----------------
-- hyper mode --
----------------

local hyperModeBindings = {
  -- { key = 'b', fn = screen.setBrightness(0.8), desc = 'Set brightness to 80%.' },
  { key = 'u', fn = mounts.unmountAll, desc = 'Unmount all volumes' },
  -- { key = 'h', fn = audio.current, desc = 'Current song' },
  -- { key = 'i', fn = audio.changeVolume(-5), desc = 'Decrease the volume by 5%' },
  -- { key = 'j', fn = audio.next, desc = 'Next song' },
  -- { key = 'k', fn = audio.previous, desc = 'Previous song' },
  -- { key = 'o', fn = audio.setVolume(15), desc = 'Default volume level' },
  -- { key = 'p', fn = audio.setVolume(30), desc = 'High volume level' },
  { key = 'r', fn = reload.reload, desc = 'Reload hammerspoon' },
  { key = 'c', fn = hs.toggleConsole, desc = 'Console' },
  -- { key = 'space', fn = audio.playpause, exitMode = true, desc = 'Pause or resume' },
  -- { key = 'u', fn = audio.changeVolume(5), desc = 'Increase the volume by 5%' },
  -- { key = 'm', fn = audio.changeVolume(-100), desc = 'Mute'},
  { key = 'v', fn = clipboard.toggle, desc = 'Clipboard'},
  { key = 'e', fn = emacs.everywhere, desc = 'Emacs Everywhere'},
}

----------------
-- org mode --
----------------

local orgModeBindings = {
  { key = 'r', fn = function () emacs.capture('roam-ref') end, desc = 'Roam Capture' },
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
