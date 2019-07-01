local hotkey = require 'hs.hotkey'
local alert = require 'hs.alert'
local fnutils = require "hs.fnutils"
local application = require "hs.application"
local keybinder = require "keybinder"

local mod = {}

alert.defaultStyle['radius'] = 5
alert.defaultStyle['textSize'] = 20

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
      keybinder.globalFilter[app] = keybinder.globalFilter[app] or {}
      table.insert(keybinder.globalFilter[app], mode.k)
    end
  end
end

return mod
