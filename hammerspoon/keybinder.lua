local application = require 'hs.application'
local window = require 'hs.window'
local fnutils = require 'hs.fnutils'
local hotkey = require 'hs.hotkey'
local inspect = require "hs.inspect"
local logger = hs.logger.new('keybinder', 'info')
local windows = require 'windows'

local hyper = { 'cmd', 'alt', 'ctrl' }
local hyperShift = { 'cmd', 'alt', 'ctrl', 'shift' }
local globalBindings = '*'
local globalFilter = {}

hotkey.alertDuration = 2.5

local mod = {
  cmd = { 'cmd' },
  hyper = hyper,
  globalBindings = globalBindings
}

function enableBindings(bindings, window)
  for _, binding in pairs(bindings) do
    binding.hotkey:enable()
  end
end

function disableBindings(bindings)
  for _, binding in pairs(bindings) do
    binding.hotkey:disable()
  end
end

local function buildBindFunction(binding)
  if binding.pos and binding.targetScreen then
    return windows.setPosition(binding.pos, binding.targetScreen, binding.reversable)
  elseif binding.pos then
    return windows.setPosition(binding.pos, 'primary', binding.reversable)
  elseif binding.name then
    return windows.launchOrCycleFocus(binding.name)
  elseif binding.fn then
    return binding.fn
  end
end

local function bind(binding)
  modifiers = binding.modifiers
  if modifiers == nil then
    modifiers = hyper
    if binding.shift then
      modifiers = hyperShift
    end
  end
  local fn = buildBindFunction(binding)
  if fn == nil then
    logger.ef('Missing binding function for: %s', inspect(binding))
  end
  binding.hotkey = hotkey.new(modifiers, binding.key, fn, message)

  -- 'filter' is a list of app to filter out, e.g. disable the global keybinding
  -- for that particular app
  if binding.filter ~= nil then
    for _, app in ipairs(binding.filter) do
      -- instantiate the list first
      globalFilter[app] = globalFilter[app] or {}
      table.insert(globalFilter[app], binding.hotkey)
    end
  end

  return binding
end

function initWatcher(appBindingMap)
  local activated = {}
  activated[application.watcher.activated] = true
  activated[application.watcher.launched] = true
  activated[application.watcher.launching] = true
  activated[application.watcher.unhidden] = true

  return
    application.watcher.new(
      function(appName, event, appObj)

        -- re-enable global keybindings and disable filtered ones; only if we're
        -- activating an app
        if activated[event] ~= nil then
          -- loop over the filtered apps
          for app, filter in pairs(globalFilter) do
            for _, key in ipairs(filter) do
              -- re-enable all global keys (see below)
              key:enable()
            end
          end

          -- needs to be after re-enabling
          for app, filter in pairs(globalFilter) do
            for _, key in ipairs(filter) do
              if app == appName then
                -- disable the specified hotkeys
                key:disable()
              end
            end
          end
        end

        local bindings = appBindingMap[appName]
        if bindings == nil then
          return
        end

        if activated[event] ~= nil then
          logger.df('Enabling for %s', appName)
          enableBindings(bindings, window.focusedWindow())
          return
        end

        logger.df('Disabling for %s', appName)
        disableBindings(bindings)
    end)
end

function mod.init(appBindingList)
  local appBindingMap = {}
  for _, app in ipairs(appBindingList) do
    appBindingMap[app.name] = fnutils.imap(app.bindings, bind)
  end
  enableBindings(appBindingMap[globalBindings])
  initWatcher(appBindingMap):start()
end

return mod
