local task = require "hs.task"
local fnutils = require "hs.fnutils"
local alert = require "hs.alert"
local logger = hs.logger.new('execute', 'debug')

local mod = {}

local function file_exists(name)
   local f=io.open(name,"r")
   if f~=nil then io.close(f) return true else return false end
end

function mod.read_file(path)
    local file = io.open(path, "rb") -- r read mode and b binary mode
    if not file then return nil end
    local content = file:read("*a") -- *a or *all reads the whole file
    file:close()
    return content
end

local function emacs_server_file()
    local home = os.getenv("HOME")
    local ret = home .. "/.emacs.d/server/server"
    if file_exists(home .. "/.emacs.d/.local/cache/server/server")
    then
        ret = home .. "/.emacs.d/.local/cache/server/server"
    end
    return ret
end

local running_processes = {}
local HOME = os.getenv('HOME')
local env = {
  PATH = '/usr/local/bin:/usr/bin:/bin',
  LC_ALL = 'en_US.UTF-8',
  LANG = 'en_US.UTF-8',
  HOME = HOME,
}

local function returnCallback(exitCode, stdOut, stdErr)
  if exitCode ~= 0 then
    alert.show('the process failed to run, see logs')
    logger.d(stdOut)
    logger.d(stdErr)
    return false
  end
  logger.f('process done')
  return true
end

local function streamCallback(_, stdOut, stdErr)
  logger.d(stdOut)
  logger.d(stdErr)
  return true
end

function getn (t)
  if type(t.n) == "number" then return t.n end
  local max = 0
  for i, _ in t do
    if type(i) == "number" and i>max then max=i end
  end
  return max
end

local function cleanupProcess(task)
  logger.df("Checking if still running: %s", task:pid())
  return task:isRunning()
end

local function expand(path)
  return path:gsub('~', HOME)
end

local function stream_default(task, stdOut, stdErr)
  return task:isRunning()
end

function mod.start(cmd, args, pwd, stream_callback, callback)
  running_processes = fnutils.filter(running_processes, cleanupProcess)

  cmd = expand(cmd)
  args = fnutils.map(args or {}, expand)
  pwd = expand(pwd or '~')

  process = task.new(cmd, callback,
                     stream_callback or stream_default,
                     args)

  table.insert(running_processes, process)

  logger.f('starting %s %s in %s', cmd, args, pwd)
  process:setWorkingDirectory(pwd)

  -- set this here so that it's always fresh
  env["EMACS_SERVER_FILE"] = emacs_server_file()

  process:setEnvironment(env)

  if not process:start() then
    logger.df('failed to start %s', cmd)
  end

  return process
end

function mod.capture(cmd, raw)
  local f = assert(io.popen(cmd, 'r'))
  local s = assert(f:read('*a'))
  f:close()
  if raw then return s end
  s = string.gsub(s, '^%s+', '')
  s = string.gsub(s, '%s+$', '')
  -- s = string.gsub(s, '[\n\r]+', ' ')
  return s
end

return mod
