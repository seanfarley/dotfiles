local application = require "hs.application"
local eventtap = require 'hs.eventtap'
local http = require "hs.http"
local logger = hs.logger.new('emacs', 'debug')
local osascript = require "hs.osascript"
local process = require "utils/process"
local selection = require "selection"
local window = require "hs.window"

local mod = {}

-- needs to be a wrapper script that will spawn a background process so that
-- hammerspoon won't crash
local ec = "~/.ec"

-- this is such horseshit: the below should work but it seems hammerspoon's lua
-- is buggy as shit when it comes to spawning processes and callbacks; this
-- forces us to re-architect everything to go back to j-martin's implementation
-- but 1) send the application that was last focused and 2) disallow spawning a
-- new frame when one is already active

-- BONUS:
-- this has the bonus of not having the emacs fullscreen frame flash (yay!) (...
-- well, flash as much)

local appRequestingEmacs = nil

-- map emacs keybindings in everything but the emacs app
local esc = hs.hotkey.new({"ctrl"}, "g", function()
      hs.eventtap.keyStroke({}, "escape")
end)

-- after much research, applicationWatcher is the best way to achieve what we
-- want for ctrl-g with emacs
local function applicationWatcher(appName, eventType, appObject)
   if (eventType == hs.application.watcher.activated) then
      if (appName == "Emacs") then
         esc:disable()
      else
         esc:enable()
      end
   end
end
local appWatcher = hs.application.watcher.new(applicationWatcher)
appWatcher:start()

function backFromEmacs()
   if appRequestingEmacs == nil then
      hs.alert("Emacs not previously requested")
      return
   end
   if string.match("Emacs", appRequestingEmacs:bundleID()) then
      -- No need to bounce back to Emacs if invoked from Emacs.
      return
   end
   appRequestingEmacs:activate()
   appRequestingEmacs = nil
end

local function eval(sexp)
  appRequestingEmacs = hs.application.frontmostApplication()

  process.start(ec, {'-n', '--quiet', '--eval', sexp})
end

local function evalInCurrentBuffer(sexp)
  eval('(with-current-buffer (window-buffer (selected-window)) ' .. sexp ..')')
end

local function open(url)
  process.start('emacsclient', { '--no-wait', '--quiet', '--suppress-output', url })
  application.launchOrFocus('Emacs')
end

function mod.helmBuffers()
  eval('(helm-mini)')
end

function mod.inbox()
  eval('(jm/open-inbox)')
end

function mod.references()
  eval('(jm/open-references)')
end

function mod.orgRifle()
  eval('(helm-org-rifle)')
end

function mod.workInbox()
  eval('(jm/open-work)')
end

function mod.agenda()
  eval('(org-agenda-list)')
end

function mod.capture(captureTemplate)
  return function()
    local focusedWindow = window.focusedWindow()
    local focusedApplication = focusedWindow:application()

    if focusedApplication:name() == 'Emacs' then
      evalInCurrentBuffer('(org-capture)')
      return
    end

    local title = focusedWindow:title() .. " - " .. focusedApplication:name()
    local url = focusedApplication:path()
    local body = selection.getSelectedText()

    if focusedApplication:name() == 'Google Chrome' then
      _, title, _ = osascript.javascript("Application('Google Chrome').windows[0].activeTab().title()")
      _, url, _ = osascript.javascript("Application('Google Chrome').windows[0].activeTab().url()")
    end

    if focusedApplication:name() == 'Finder' then
      _, title, _ = osascript.javascript("Application('Finder').selection()[0].name()")
      _, url, _ = osascript.javascript("Application('Finder').selection()[0].url()")
    end

    local protocolUrl = 'org-protocol://capture?' ..
      'title=' .. http.encodeForQuery(title) ..
      '&url=' .. http.encodeForQuery(url) ..
      '&body=' .. http.encodeForQuery(body or '')
    if captureTemplate then
      protocolUrl = protocolUrl .. '&template=' .. captureTemplate
    end

    logger.df("URL: %s", protocolUrl)
    open(protocolUrl)
  end
end

function mod.test_frame()
  frameIsRunning = eval('(smf/test-frame)')
end

function mod.app_loader()
  eval('(counsel-mac-app-frame)')
end

return mod
