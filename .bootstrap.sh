#!/bin/bash -
#===============================================================================
#
#          FILE:  .bootstrap.sh
#
#         USAGE:  ./.bootstrap.sh
#
#   DESCRIPTION:  Copies over files from an initial clone of 'settings'
#
#       OPTIONS:  ---
#  REQUIREMENTS:  ---
#          BUGS:  ---
#         NOTES:  ---
#        AUTHOR:  Sean Farley
#       COMPANY:
#       CREATED:  01/19/2012 01:08:31 AM CST
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error

# if accidentally ran from $HOME, then change directories
[[ $PWD == $HOME ]] && cd settings

for file in .ssh/*; do
  mv $file $HOME/.ssh
done

[[ -d .ssh ]] && rm -rf .ssh

for file in .[a-zA-Z0-9]*; do
  [[ -d $HOME/$file ]] && rm -rf $HOME/$file
  mv $file $HOME
done
