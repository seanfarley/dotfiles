local eventtap = require 'hs.eventtap'
local mouse = require 'hs.mouse'
local screen = require 'hs.screen'
local timer = require 'hs.timer'
local window = require 'hs.window'
local windows = require 'windows'

local mod = {}

mod.name = {
  activityMonitor = 'Activity Monitor',
}

local states = {
  noisyTyperEnabled = false
}

local function wait(n)
  local n = n or 1
  -- 0.01s
  timer.usleep(10000 * n)
end

function mod.switchToAndType(application, modifiers, keyStroke, delay)
  windows.launchOrCycleFocus(application)()
  wait(delay)
  eventtap.keyStroke(modifiers, keyStroke)
end

local function clickNotification(offset)
  local currentScreen = mouse.getCurrentScreen()
  local currentPos = mouse.getRelativePosition()
  local targetScreen = screen.primaryScreen()
  local targetPos = { x = targetScreen:frame().w - offset, y = 40 }

  mouse.setRelativePosition(targetPos, targetScreen)
  eventtap.leftClick(targetPos)
  mouse.setRelativePosition(currentPos, currentScreen)
end

function mod.openNotification()
  clickNotification(160)
end

function mod.openNotificationAction()
  clickNotification(40)
end

function mod.activityMonitor()
  mod.switchToAndType(mod.name.activityMonitor, {'cmd'}, '2')
  local win = hs.window.focusedWindow()

  win:moveToUnit({ 0.85, 0.9, 0.1, 0.1 }, 0)
  eventtap.keyStroke({'cmd'}, '1')
end

return mod
