local uielement = require "hs.uielement"
local timer = require "hs.timer"
local pasteboard = require "hs.pasteboard"
local http = require "hs.http"
local task = require "hs.task"
local eventtap = require "hs.eventtap"
local alert = require "hs.alert"
local window = require "hs.window"
local osascript = require "hs.osascript"
local logger = hs.logger.new('selection', 'debug')

local mod = {}

local engines = {
  google = 'https://www.google.ca/search?q=',
}

local function selectedTextFromClipboard(currentApp)
  local selection
  local function getClipboard(initial, retries)
    if retries < 0 then return initial end
    timer.usleep(0.1 * 1000000)
    local selection = pasteboard.readString()
    if selection == initial and currentApp ~= 'Google Chrome' then
      logger.d('Same result. Retrying')
      return getClipboard(initial, retries - 1)
    else
      return selection
    end
  end

  local initial = pasteboard.readString()
  eventtap.keyStroke({'cmd'}, 'c')
  selection = getClipboard(initial, 3)
  logger.df('clipboard: %s', selection)

  if initial == selection then
    -- if we haven't selected anything then don't make the (random) contents of
    -- the pasteboard the current selection; the downside of this, of course, is
    -- that if the selection *happens* to be the same as the pasteboard then
    -- this will make it seem as nothing is selected
    selection = ""
  else
    -- set pastboard using 'pbcopy' due to unreliability of pastboard:setContents
    local f = io.popen("pbcopy", 'w')
    f:write(initial)
    f:close()
  end

  return selection
end

function mod.getSelectedText()
  local currentApp = window.focusedWindow():application():name()
  local element  = uielement.focusedElement()
  local selection

  if element then
    selection = element:selectedText()
  end

  -- -- This avoids using the pasteboard but is *significantly* slower
  -- if currentApp == "Safari" or currentApp == "Google Chrome" then
  --   local currentTab = "currentTab"
  --   if currentApp == "Google Chrome" then
  --     currentTab = "activeTab"
  --   end
  --   _, text, _ = osascript.javascript("Application('" .. currentApp .. "').doJavaScript('window.getSelection().toString();', { in: Application('" .. currentApp .. "').windows[0]." .. currentTab .. " })")
  --   return text
  -- end

  if not selection or currentApp == 'Emacs' then
    -- NOTE this will make a beep noise with some apps (e.g. Safari) if nothing
    -- is selected
    return selectedTextFromClipboard(currentApp)
  end

  return selection
end

local function openUrl(url)
  task.new('/usr/bin/open', nil, function() end, {url}):start()
end

local function query(url, text)
  openUrl(url .. http.encodeForQuery(text))
end

local function google(text, engine)
  query(engines[engine], text or mod.getSelectedText())
end

function mod.actOn(engine)
  return function()
    local text = mod.getSelectedText()
    if text:gmatch("https?://")() then
      openUrl(text)
    -- TODO: Cleanup silly regex
    elseif text:gmatch("1%d%d%d%d%d%d%d%d%d+")() then
      mod.epochSinceNow(text)
    else
      google(text, engine)
    end
  end
end

function mod.paste()
  local content = pasteboard.getContents()
  alert("Pasting/Typing: '" .. content .. "'")
  eventtap.keyStrokes(content)
end

local function round(number)
  return tostring(math.floor(number))
end

function mod.epochSinceNow(text)
  local initial = timer.secondsSinceEpoch()
  local selection = tonumber(text or mod.getSelectedText())

  if selection > 1000000000000 then
    selection = selection / 1000
  end

  local diff = initial - selection
  alert.show(
    round(diff / 60) .. ' mins / ' ..
      round(diff / 60 / 60) .. ' hours / ' ..
      round(diff / 60 / 60 / 24) .. ' days / ' ..
      round(diff / 60 / 60 / 24 / 30) .. ' months ago'
  )
end

return mod
