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

-- this is such horseshit: the below should work but it seems hammerspoon's lua
-- is buggy as shit when it comes to spawning processes and callbacks; this
-- forces us to re-architect everything to go back to j-martin's implementation
-- but 1) send the application that was last focused and 2) disallow spawning a
-- new frame when one is already active

-- BONUS:
-- this has the bonus of not having the emacs fullscreen frame flash (yay!) (...
-- well, flash as much)

local appRequestingEmacs = nil

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
  hs.application.launchOrFocus("Emacs")
end

local function evalInCurrentBuffer(sexp)
  eval('(with-current-buffer (window-buffer (selected-window)) ' .. sexp ..')')
end

local function open(url)
  appRequestingEmacs = hs.application.frontmostApplication()

  process.start(ec, { '--no-wait', '--quiet', '--suppress-output', url })
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
  eval('(smf/org-agenda)')
end

function mod.mu4e()
  eval('(=mu4e)')
end

function mod.vterm()
  eval('(=vterm)')
end

function mod.irc()
  eval('(=irc)')
end

function mod.switchWorkspace()
  eval('(smf/activate-emacs)')
end

function mod.capture(captureTemplate)
  local focusedWindow = window.focusedWindow()
  local focusedApplication = focusedWindow:application()
  
  if focusedApplication:name() == 'Emacs' then
    eval('(smf/org-capture)')
    return
  end

  if focusedApplication:name() == 'Slack' then
    captureTemplate = 's'
  end

  local title = focusedWindow:title() .. " - " .. focusedApplication:name()
  local url = focusedApplication:path()
  local body = selection.getSelectedText()

  if focusedApplication:name() == 'Google Chrome' then
    _, url, _ = osascript.javascript("Application('Google Chrome').windows[0].activeTab().url()")
  end

  if focusedApplication:name() == 'Safari' then
    _, url, _ = osascript.javascript("Application('Safari').windows[0].currentTab().url()")
  end

  if focusedApplication:name() == 'Finder' then
    _, title, _ = osascript.javascript("Application('Finder').selection()[0].name()")
    _, url, _ = osascript.javascript("Application('Finder').selection()[0].url()")
  end

  if captureTemplate == nil then
    captureTemplate = 't'
  end

  local protocolUrl = 'org-protocol://capture?' ..
    'title=' .. http.encodeForQuery(title) ..
    '&url=' .. http.encodeForQuery(url) ..
    '&body=' .. http.encodeForQuery(body or '') ..
    '&template=' .. captureTemplate

  logger.df("URL: %s", protocolUrl)
  open(protocolUrl)
end

function mod.test_frame()
  frameIsRunning = eval('(smf/test-frame)')
end

function mod.app_loader()
  eval('(counsel-mac-app-frame)')
end

return mod
