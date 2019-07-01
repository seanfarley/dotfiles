local hotkey = require 'hs.hotkey'
local alert = require 'hs.alert'
local fnutils = require "hs.fnutils"
local application = require "hs.application"

local mod = {}
local globalFilter = {}

alert.defaultStyle['radius'] = 5
alert.defaultStyle['textSize'] = 20

local function initWatcher(appBindingMap)
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

      end
    )
end

-- bindings { { key = 'string', fn = fn } }
function mod.create(modifiers, key, name, bindings, filter)
  local mode = hotkey.modal.new(modifiers, key)

  local function buildDesc(binding)
    local desc = binding.desc or binding.name
    return binding.key .. " \t→\t " .. desc
  end

  function mode:entered()
    local d = name .. ' Mode'
    local mappings = fnutils.imap(bindings, buildDesc)
    if mappings ~= nil then
      d = d .. '\n\n' .. table.concat(mappings, "\n")
    end
    alert.show(d, 120)
  end

  local function exit()
    mode:exit()
  end

  function mode:exited()
    alert.closeAll()
  end

  local function callAndExit(fn)
    return function()
      exit()
      fn()
    end
  end

  local function bindFn(binding)
    local message = binding.desc or binding.name
    local fn = function()
      if message ~= nil then
        alert.show(message, 0.75)
      end
      if binding.fn then
        return binding.fn()
      end
      if binding.name then
        return application.open(binding.name)()
      end
    end
    if binding.exitMode then
      mode:bind(modifiers, binding.key, callAndExit(fn))
    else
      mode:bind(modifiers, binding.key, fn)
    end
    mode:bind({}, binding.key, callAndExit(fn))
  end

  fnutils.each(bindings, bindFn)
  mode:bind({}, 'escape', exit)
  mode:bind({}, 'q', exit)
  mode:bind({'ctrl'}, 'g', exit)
  mode:bind({'ctrl'}, '[', exit)
  mode:bind(modifiers, key, exit)

  -- 'filter' is a list of app to filter out, e.g. disable the global keybinding
  -- for that particular app
  if filter ~= nil then
    for _, app in ipairs(filter) do
      -- instantiate the list first
      globalFilter[app] = globalFilter[app] or {}
      table.insert(globalFilter[app], mode.k)
    end
  end


  initWatcher(nil):start()
end

return mod
