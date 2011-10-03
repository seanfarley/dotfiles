#!/bin/bash - 
#===============================================================================
#
#          FILE:  hgd.sh
# 
#         USAGE:  ./hgd.sh 
# 
#   DESCRIPTION:  
#   This script is meant to run in the background to check a list of mercurial
#   repositories for their status. The goal is to have this run independently
#   of the BASH command prompt (python is too slow to put in there)
# 
#       OPTIONS:  ---
#  REQUIREMENTS:  ---
#          BUGS:  ---
#         NOTES:  ---
#        AUTHOR: Sean Farley
#       COMPANY: 
#       CREATED: 09/30/2011 19:28:18 CDT
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error

function hg_status {
    if [ -z "$(eval hg st -mard --cwd $1)" ]; then
      eval rm -f $repo/.hg/dirty
      echo "clean"
    else
      eval touch $repo/.hg/dirty
      echo "dirty"
    fi
}

while [ 1 ]; do
  while read repo; do
    (hg_status $repo &)
  done < $HOME/.hgrepos
  sleep 1
done
