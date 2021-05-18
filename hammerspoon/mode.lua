local hotkey = require 'hs.hotkey'
local alert = require 'hs.alert'
local fnutils = require "hs.fnutils"
local application = require "hs.application"
local keybinder = require "keybinder"

local mod = {}

alert.defaultStyle['radius'] = 5
alert.defaultStyle['textSize'] = 20

-- bindings { { key = 'string', fn = fn } }
function mod.create(modifiers, key, name, bindings, filter, delete)
  local mode = hotkey.modal.new(modifiers, key)

  local function buildDesc(binding)
    local desc = binding.desc or binding.name
    local key = binding.key
    local mods = ''

    if key == 'space' then
      key = "' '"
    end
    if binding.modifiers then
      if fnutils.contains(binding.modifiers, "ctrl") then
        mods = mods .. "C-"
      end
      if fnutils.contains(binding.modifiers, "alt") then
        mods = mods .. "M-"
      end
      if fnutils.contains(binding.modifiers, "shift") then
        mods = mods .. "S-"
      end
      if fnutils.contains(binding.modifiers, "cmd") then
        mods = mods .. "s-"
      end
    end
    return mods .. key .. " \t→\t " .. desc
  end

  function mode:entered()
    local d = name .. ' Mode'
    local mappings = fnutils.imap(bindings, buildDesc)

    -- https://github.com/asmagill/hammerspoon-config/blob/master/_scratch/modalSuppression.lua
    local eventtap = require("hs.eventtap")
    local passThroughKeys = {}

    for i,v in ipairs(self.keys) do
      -- parse for flags, get keycode for each
      local kc, mods = tostring(v._hk):match("keycode: (%d+), mods: (0x[^ ]+)")
      local hkFlags = tonumber(mods)
      local hkOriginal = hkFlags
      local flags = 0
      if (hkFlags &  256) ==  256 then hkFlags, flags = hkFlags -  256, flags | eventtap.event.rawFlagMasks.command   end
      if (hkFlags &  512) ==  512 then hkFlags, flags = hkFlags -  512, flags | eventtap.event.rawFlagMasks.shift     end
      if (hkFlags & 2048) == 2048 then hkFlags, flags = hkFlags - 2048, flags | eventtap.event.rawFlagMasks.alternate end
      if (hkFlags & 4096) == 4096 then hkFlags, flags = hkFlags - 4096, flags | eventtap.event.rawFlagMasks.control   end
      if hkFlags ~= 0 then print("unexpected flag pattern detected for " .. tostring(v._hk)) end
      passThroughKeys[tonumber(kc)] = flags
    end

    self._eventtap = eventtap.new(
      {
        eventtap.event.types.keyDown,
        eventtap.event.types.keyUp,
      }, function(event)
        -- check only the flags we care about and filter the rest
        local flags = event:getRawEventData().CGEventData.flags  & (
          eventtap.event.rawFlagMasks.command   |
          eventtap.event.rawFlagMasks.control   |
          eventtap.event.rawFlagMasks.alternate |
          eventtap.event.rawFlagMasks.shift
                                                                   )
        if passThroughKeys[event:getKeyCode()] == flags then
          hs.printf("passing:     %3d 0x%08x", event:getKeyCode(), flags)
          return false -- pass it through so hotkey can catch it
        else

          mode:exit()

          for _, key in ipairs(keybinder.globalMap[keybinder.globalBindings]) do
            if key.key == hs.keycodes.map[event:getKeyCode()] then
              local t1 = {}
              if (flags & eventtap.event.rawFlagMasks.command) ~= 0 then
                t1[#t1 + 1] = "cmd"
              end
              if (flags & eventtap.event.rawFlagMasks.control) ~= 0 then
                t1[#t1 + 1] = "ctrl"
              end
              if (flags & eventtap.event.rawFlagMasks.alternate) ~= 0 then
                t1[#t1 + 1] = "alt"
              end
              if (flags & eventtap.event.rawFlagMasks.shift) ~= 0 then
                t1[#t1 + 1] = "shift"
              end

              if (key.modifiers == nil or #key.modifiers == 0) and (#t1 == 0) then
                return false
              end

              if #key.modifiers > 0 and #t1 == #key.modifiers then
                table.sort(key.modifiers)
                table.sort(t1)
                for i, mod in ipairs(key.modifiers) do
                  if mod ~= t1[i] then
                    hs.printf("suppressing: %3d 0x%08x", event:getKeyCode(), flags)
                    return true
                  end
                end

                -- getting here means all flags / modifiers are equal
                return false
              end
            end
          end

          hs.printf("suppressing: %3d 0x%08x", event:getKeyCode(), flags)
          return true -- delete it if we got this far -- it's a key that we want suppressed
        end
    end)

    mode._eventtap:start()

    if mappings ~= nil then
      d = d .. '\n\n' .. table.concat(mappings, "\n")
    end
    alert.show(d, 120)
  end

  local function exit()
    mode:exit()
  end

  function mode:exited()
    mode._eventtap:stop()
    alert.closeAll()
    if delete then
      mode:delete()
    end
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
      if binding.fn then
        return binding.fn()
      end
      if binding.name then
        return application.open(binding.name)()
      end
    end
    mode:bind(binding.modifiers, binding.key, callAndExit(fn))
  end

  fnutils.each(bindings, bindFn)
  mode:bind({}, 'escape', exit)
  mode:bind({}, 'q', exit)
  mode:bind({'ctrl'}, 'g', exit)
  mode:bind({'ctrl'}, '[', exit)

  keybinder.bindExclude(mode.k, filter)

  return mode
end

return mod
