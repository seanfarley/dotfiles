import commands
def get_keychain_pass(account=None, server=None):
    params = {
        'security': '/usr/bin/security',
        'command': 'find-internet-password',
        'account': account,
        'server': server
    }

    command = ("%(security)s %(command)s -w -a %(account)s "
               "-s %(server)s") % params
    return commands.getoutput(command)
