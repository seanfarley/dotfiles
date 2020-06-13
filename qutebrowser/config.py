import os
import re
import subprocess
import sys

config = config                 # type: ConfigAPI # noqa: F821
c = c                           # type: ConfigContainer # noqa: F821

# config.source('qutemacs.py')
config.source('qutemacs2.py')

c.downloads.remove_finished = 1000
c.auto_save.session = True
c.content.pdfjs = True
c.content.plugins = True

# Input.
# c.input.insert_mode.auto_leave = True
# c.input.insert_mode.auto_load = True
# c.input.insert_mode.plugins = True

config.bind('<meta-w>', 'tab-close')
config.bind('<meta-t>', 'set-cmd-text -s :open -t')
config.bind('<meta-l>', 'set-cmd-text -s :open')

config.bind('<meta-=>', 'zoom-in')
config.bind('<meta-->', 'zoom-out')
config.bind('<meta-0>', 'zoom')

config.bind('<meta-shift-[>', 'tab-prev')
config.bind('<meta-shift-]>', 'tab-next')

config.bind('<meta-[>', 'back')
config.bind('<meta-]>', 'forward')

config.bind('<meta-r>', 'reload')

c.fonts.tabs = "11pt monospace"
c.tabs.padding = {
    "left": 5,
    "right": 5,
    "top": 2,
    "bottom": 2,
}

config.source('nord-qutebrowser.py')

# KEEPING FOR EXAMPLE
# launching from spotlight doesn't add /usr/local/bin to the path (which is
# needed for the bw command line to find node and friends)
# os.environ["PATH"] = "/usr/local/bin:" + os.environ.get("PATH", "")

# session_re = re.compile(r'\bBW_SESSION="(\S+)"')
# cmd = ("bw unlock "
#        "$(/usr/bin/security find-internet-password -w "
#        "-a sean@farley.io -s bitwarden.farley.in)")
# proc = subprocess.run(cmd, shell=True,
#                       stdout=subprocess.PIPE,
#                       stderr=subprocess.PIPE,
#                       encoding='utf8')
# session = session_re.search(proc.stdout).group(1)
# qcmd = "spawn --userscript qute-bitwarden -n --session {}".format(session)

# cmd = "~/.edit.sh -n --eval \"'(bitwarden-unlocked-p)'\""
# proc = subprocess.run(cmd, shell=True,
#                       stdout=subprocess.PIPE,
#                       stderr=subprocess.PIPE,
#                       encoding='utf8')

# qcmd = ("spawn --userscript "
#         "qute-bitwarden -n --session {}".format(proc.stdout.strip()))
# config.bind('<meta-i>', qcmd)
