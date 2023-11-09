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

function mod.focus()
  local c = "/bin/sh -c 'dirname `realpath /usr/local/bin/emacs`'"
  local emacs = process.capture(c) .. "/../Emacs.app"
  -- hs.alert(emacs)
  hs.application.launchOrFocus(emacs)
end

function mod.client()
  -- find homebrew emacsclient
  ec = "/usr/local/bin/"
  if process.file_exists(ec .. "emacsclient") then
    -- hs.alert("found it at: " .. ec)
    return ec .. "emacsclient"
  end

  local ec = "/Applications/Emacs.app/../bin/"
  if process.file_exists(ec .. "emacsclient") then
    -- hs.alert("found it at: " .. ec)
    return ec .. "emacsclient"
  end

  return
end

local function client_callback(task, stdout, stderr)
  -- hs.alert("Exit code: " .. exit_code)
  -- hs.alert("Stdout: " .. stdout)
  local chk_err = ": error accessing server file"
  if stderr ~= nil then
    local start = string.find(stderr, chk_err)
    if start ~= nil then
      hs.alert("E" .. stderr:sub(start + 3))
    end
  end

  if task ~= nil then
    return task:isRunning()
  end
  return false
end

function mod.eval(sexp, nofocus)
  ec = mod.client()
  if ec == nil then
    hs.alert("Could not find emacsclient!")
  end

  appRequestingEmacs = hs.application.frontmostApplication()
  process.start(ec, {'-n', '--quiet', '--eval', sexp},
                nil, client_callback)

  -- yuck, two different behaviors: 1) focus emacs and keep it there, 2) don't
  -- focus (e.g. a popup frame) then switch back to original app
  if nofocus == nil then
    mod.focus()
  else
    hs.application.launchOrFocus(appRequestingEmacs)
  end
end

local function evalInCurrentBuffer(sexp)
  eval('(with-current-buffer (window-buffer (selected-window)) ' .. sexp ..')')
end

local function open(url)
  ec = mod.client()
  appRequestingEmacs = hs.application.frontmostApplication()

  process.start(ec, { '--no-wait', '--quiet', '--suppress-output', url })
  mod.focus()
end

function mod.sendKey(k)
  -- https://emacs.stackexchange.com/questions/2461/how-can-i-simulate-an-arbitary-key-event-from-elisp
  mod.eval('(setq unread-command-events (listify-key-sequence "' .. k .. '"))')
end

function mod.todolist()
  mod.eval('(org-todo-list)')
end

function mod.agenda()
  mod.eval('(org-agenda-list)')
end

function mod.mu4e()
  mod.eval('(=mu4e)')
end

function mod.vterm()
  mod.eval('(=vterm)')
end

function mod.mpc()
  mod.eval('(=mpc)')
end

function mod.irc()
  mod.eval('(=irc)')
end

function mod.everywhere()
  mod.eval('(emacs-everywhere)', true)
end

function mod.matrix()
  mod.eval('(smf/matrix)')
end

function mod.orgroam()
  mod.eval('(=org-roam)')
end

function mod.switchWorkspace()
  mod.eval('(smf/activate-emacs)')
end

function mod.capture(protocol, captureTemplate, time)
  local focusedWindow = window.focusedWindow()
  local focusedApplication = focusedWindow:application()

  local title = focusedWindow:title() .. " - " .. focusedApplication:name()
  local url = focusedApplication:path()
  local urlkey = "url"
  local body = selection.getSelectedText()
  local timekey = ""

  if focusedApplication:name() == 'Safari' then
    _, url, _ = osascript.javascript("Application('Safari').windows[0].currentTab().url()")

    -- remove ' - Safari' from the end of the title for webpages
    title = title:gsub(" [-] Safari$", "")
  end

  if focusedApplication:name() == 'Finder' then
    _, title, _ = osascript.javascript("Application('Finder').selection()[0].name()")
    _, url, _ = osascript.javascript("Application('Finder').selection()[0].url()")
  end

  if captureTemplate == nil then
    captureTemplate = 't'
  end

  if protocol == nil then
    protocol = "capture"
  end

  if protocol == "roam-ref" then
    urlkey = "ref"
  end

  if time then
    body = time
  end

  local protocolUrl = 'org-protocol://' .. protocol .. '?' ..
    'title=' .. http.encodeForQuery(title) ..
    '&' .. urlkey .. '=' .. http.encodeForQuery(url) ..
    '&body=' .. http.encodeForQuery(body or '') ..
    '&template=' .. captureTemplate

  hs.printf("URL: %s", protocolUrl)
  open(protocolUrl)
end

function mod.test_frame()
  frameIsRunning = eval('(smf/test-frame)')
end

function mod.app_loader()
  eval('(counsel-mac-app-frame)')
end

return mod
