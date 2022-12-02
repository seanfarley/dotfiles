-- initially from https://raw.githubusercontent.com/oskarols/dotfiles/0bd44443d00108e3c1a8d01520489e2d165f70ff/hammerspoon/extensions.lua

local fnutils = require "hs.fnutils"
local indexOf = fnutils.indexOf
local filter = fnutils.filter
local geometry = require "hs.geometry"
local window = require "hs.window"
local drawing = require "hs.drawing"
local timer = require "hs.timer"
local mouse = require "hs.mouse"
local fs = require "hs.fs"
local application = require "hs.application"
local screen = require "hs.screen"
local logger = hs.logger.new('windows.ext', 'debug')

local mod = {}

application.enableSpotlightForNameSearches(true)
---------------------------------------------------------
-- functools
---------------------------------------------------------

local function isFunction(a)
  return type(a) == "function"
end

-- gets propery or method value
-- on a table
local function result(obj, property)
  if not obj then return nil end

  if isFunction(property) then
    return property(obj)
  elseif isFunction(obj[property]) then -- string
    return obj[property](obj) -- <- this will be the source of bugs
  else
    return obj[property]
  end
end

---------------------------------------------------------
-- Coordinates, points, rects, frames, tables
---------------------------------------------------------

-- Fetch next index but cycle back when at the end
--
-- > getNextIndex({1,2,3}, 3)
-- 1
-- > getNextIndex({1}, 1)
-- 1
-- @return int
local function getNextIndex(table, currentIndex)
  local nextIndex = currentIndex + 1
  if nextIndex > #table then
    nextIndex = 1
  end

  return nextIndex
end

---------------------------------------------------------
-- Mouse
---------------------------------------------------------

local mouseCircle = nil
local mouseCircleTimer = nil

function mod.mouseHighlight()
  -- Delete an existing highlight if it exists
  result(mouseCircle, "delete")
  result(mouseCircleTimer, "stop")

  -- Get the current co-ordinates of the mouse pointer
  local mousepoint = mouse.getAbsolutePosition()

  local circle = geometry.rect(mousepoint.x - 20, mousepoint.y - 20, 40, 40)

  local fillColor = { red = 0.5, blue = 0.5, green = 0.5, alpha = 0.5 }

  mouseCircle = drawing.circle(circle)
  mouseCircle:setFillColor(fillColor)
  mouseCircle:setStrokeWidth(0)
  mouseCircle:show()

  -- Set a timer to delete the circle after 3 seconds
  mouseCircleTimer = timer.doAfter(0.2, function()
    mouseCircle:delete()
  end)
end

function mod.centerOnTitle(rect)
  local point = geometry.rectMidPoint(rect)
  point.y = rect.y + 5
  mouse.absolutePosition = point
end


---------------------------------------------------------
-- Application / window
---------------------------------------------------------

-- Returns the next successive window given a collection of windows
-- and a current selected window
--
-- @param  windows  list of hs.window or applicationName
-- @param  window   instance of hs.window
-- @return hs.window
local function getNextWindow(currentWindow)
  local windows = currentWindow:application():allWindows()

  windows = filter(windows, window.isStandard)
  -- windows = filter(windows, window.isVisible)

  -- need to sort by ID, since the default order of the window
  -- isn't usable when we change the mainWindow
  -- since mainWindow is always the first of the windows
  -- hence we would always get the window succeeding mainWindow
  table.sort(windows, function(w1, w2)
    return w1:id() > w2:id()
  end)

  local lastIndex = indexOf(windows, currentWindow)

  if lastIndex == nil then
    return currentWindow
  end

  return windows[getNextIndex(windows, lastIndex)]
end

-- Needed to enable cycling of application windows
local lastToggledAppName = ''

function mod.launchOrCycleFocus(applicationName)
  return function()
    application.launchOrFocus(applicationName)
  end
end

return mod
