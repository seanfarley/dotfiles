local proc = require('utils/process')
local mod = {}

function mod.init()
  -- check to see if slack has remote debugging port enabled
  local content = proc.read_file('/Applications/Slack.app/Contents/MacOS/Slack')

  -- if the file is too big then it's the slack binary
  if #content > 1000 then
    os.rename('/Applications/Slack.app/Contents/MacOS/Slack',
              '/Applications/Slack.app/Contents/MacOS/Slack.bak')
    file = io.open('/Applications/Slack.app/Contents/MacOS/Slack', 'w')
    file:write('#!/bin/sh\n\n')
    file:write('/Applications/Slack.app/Contents/MacOS/Slack.bak --remote-debugging-port=31337')
    file:close()
    os.execute('chmod +x /Applications/Slack.app/Contents/MacOS/Slack')
    hs.alert.show("Slack enabled with remote debugging now")
  end

end

return mod
