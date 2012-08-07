#!/bin/bash

####################################################################################################
#                                             defaults                                             #
####################################################################################################

device="Wi-Fi"
proxyState=""
command="status"
address="mcs"
port="9997"
ip=""
currentlist=""

####################################################################################################
#                                            functions                                             #
####################################################################################################

# return shortname for port number
# 9997 = anl
# 9998 = iit
# 9999 = home
portname() {
  local _name=""
  case $1 in
    9997) _name="anl";;
    9998) _name="iit";;
    9999) _name="home";;
  esac
  [ -n $_name ] && echo " ($_name)"
}

portaddress() {
  local _name=""
  case $1 in
    9997) _name="login.anl.mcs.anl";;
    9998) _name="karlin.math.iit.edu";;
    9999) _name="home.seanfarley.org";;
  esac
  [ -n $_name ] && echo "$_name"
}

nameport() {
  local _port=""
  case $1 in
    anl) _port="9997";;
    iit) _port="9998";;
    home) _port="9999";;
  esac
  [ -n $_port ] && echo "$_port"
}

# get the current proxy state and open ports
state() {
  proxyState="disabled"
  [ `scutil --proxy | grep SOCKSEnable | awk '{ print $3 }'` -eq 1 ] && proxyState="enabled"
  current="$(lsof -i 4TCP@localhost -P -sTCP:LISTEN -a -c /^ssh$/)"
}

enable() {
  if [ "$proxyState" == "disabled" ]; then
    networksetup -setsocksfirewallproxy "$device" 127.0.0.1 $port off
    ssh -q -C -N -D $port $address &
    sleep 4
  fi
}

disable() {
  echo "$current" | grep $port | awk '{print $2}' | tail -1 | xargs -I{} kill {} 2> /dev/null
  if [ "$proxyState" == "enabled" ]; then
    networksetup -setsocksfirewallproxystate "$device" off
  fi
}

toggle() {
  if [ $proxyState == "enabled" ]; then
    disable
  else
    enable
  fi
}

ui() {

  if [ -e /usr/local/bin/growlnotify ]; then
    /usr/local/bin/growlnotify "$1" -m "$2"
  fi

  echo "$1"
  echo "$2"
}

####################################################################################################
#                                           main program                                           #
####################################################################################################

# if the first argument is all alphabetic, then assume it's a command
if [[ "$1" == [a-zA-Z]* ]]; then
  command="$1"
  shift 1
fi

# parse options
while getopts a:p:s: name
do
  case $name in
    a)  address="$OPTARG";;
    p)  port="$OPTARG";;
    s)  port="$(nameport $OPTARG)";
        address="$(portaddress $port)";;
    ?)  echo "usage: $0 [command] [options]

This script changes the SOCKS proxy on Mac OS X >= 10.7

COMMAND: ($command)
   status
   enable
   disable
   toggle

OPTIONS:
   -h      Show this message
   -a      Server address ($address)
   -p      Port to listen on ($port)
   -s      Shortcut name (anl, iit, or home)"
      exit 2;;
  esac
done
shift $(($OPTIND - 1))

# get the current state
state

# execute command
case $command in
  enable) enable;;
  disable) disable;;
  toggle) toggle;;
esac

# get the current state, again
state

# get the external ip
ui "Getting external IP ..." && ip="$(curl $([ "$proxyState" == "enabled" ] && echo "--socks5-hostname localhost:$port") -s http://automation.whatismyip.com/n09230945.asp)"

# format the open connections
if [ -n "$current" ]; then
  currentlist="

Connections:
$(echo "$current" | tail -n+2 | awk '{print " ",$9}' | sed -e 's,9997,9997 (ANL),' -e 's,9998,9998 (IIT),' -e 's,9999,9999 (Home),')"
fi

ui "SOCKS Proxy: $proxyState" "External IP: $ip$currentlist"
