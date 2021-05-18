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
    local key = binding.key
    if key == 'space' then
      key = "' '"
    end
    return key .. " \t→\t " .. desc
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
    mode:bind({}, binding.key, callAndExit(fn))
  end

  fnutils.each(bindings, bindFn)
  mode:bind({}, 'escape', exit)
  mode:bind({}, 'q', exit)
  mode:bind({'ctrl'}, 'g', exit)
  mode:bind({'ctrl'}, '[', exit)

  keybinder.bindExclude(mode.k, filter)
end

return mod
