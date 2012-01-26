#!/bin/bash -
#===============================================================================
#
#          FILE:  .install.sh
#
#         USAGE:  ./.install.sh
#
#   DESCRIPTION:  Install common packages
#
#       OPTIONS:  ---
#  REQUIREMENTS:  At the very least, need python including dev headers
#          BUGS:  ---
#         NOTES:  ---
#        AUTHOR:  Sean Farley
#       COMPANY:
#       CREATED:  01/19/2012 01:09:43 AM CST
#      REVISION:  ---
#===============================================================================

LOC=$HOME/.local
CURL="curl -C - -L -O"
if [[ -z $(which curl 2>/dev/null) ]]; then
  CURL="wget -c"
fi

[[ $SANDBOX ]] || SANDBOX=$HOME/projects/sandbox

[[ -d "$SANDBOX" || ! -L "$SANDBOX" ]] && mkdir -p $SANDBOX

# first argument is type
# second is URL
# third is configure options or filename to save-as for 'script'
function install() {

  PACKAGE=$(basename $2)
  EXT=""
  GET="$CURL"
  EXTRACT="true"

  if [[ $2 == git* ]]; then
    GET="git clone"
    PACKAGE=$(basename $PACKAGE .git)
  elif [[ $PACKAGE == *\.tar* ]]; then
    # Making this too general is crazy at this point, so just assume everything is .tar.* for tarballs

    EXT=".tar.${PACKAGE##*.}"
    PACKAGE=$(basename $PACKAGE $EXT)

    # Testing for zip type? ugh.
    EXTRACT="tar zxf"
    if [[ -n $(echo $EXT | grep b) ]]; then
      EXTRACT="tar jxf"
    fi
  else
    GET="hg clone"
  fi

  if [[ -n "$DEBUG" ]]; then
    echo "Type: $1"
    echo "URL: $2"
    echo "Package: $PACKAGE"
    echo "Extension: $EXT"
    echo "Exract: $EXTRACT"
    echo "Get: $GET"
  fi

  # set the commands for downloading so they can be overloaded
  DL="cd $SANDBOX; rm -rf $PACKAGE; $GET $2; $EXTRACT $PACKAGE$EXT; cd $PACKAGE"

  case $1 in
    python)
      eval $DL
      python setup.py install --prefix=$LOC
      ;;
    gnu)
      eval $DL
      ./configure --prefix=$LOC $3
      make install
      ;;
    script)
      # sigh
      # stupid, stupid code duplication!
      # just download the script then exit
      GET="curl -o"
      if [[ -z $(which curl 2>/dev/null) ]]; then
        GET="wget -O"
      fi
      $GET $3 $2
      ;;
  esac
}

# default packages to install unless a command-line option is given
PACKAGES="hg git histedit crecord hgsubversion hg-git global bash-completion pss emacs"
[[ -n "$@" ]] && PACKAGES="$@"

export PYTHONPATH=$LOC/lib/python$(python -c 'import sys; print("%i.%i" % (sys.version_info[0],sys.version_info[1]))')/site-packages:$PYTHONPATH
export PATH=$HOME/.local:$PATH

if [[ $PACKAGES == hg || $PACKAGES == *hg[!-a-zA-Z]* || $PACKAGES == *merc* ]]; then
  install python http://mercurial.selenic.com/release/mercurial-2.0.2.tar.gz
fi

if [[ $PACKAGES == git || $PACKAGES == *git[!-a-zA-Z]* ]]; then
  install gnu http://git-core.googlecode.com/files/git-1.7.8.4.tar.gz
fi

if [[ $PACKAGES == he || $PACKAGES == *he[!-a-zA-Z]* || $PACKAGES == *histedit* ]]; then
  install python http://bitbucket.org/seanfarley/histedit
fi

if [[ $PACKAGES == *crecord* ]]; then
  install python http://bitbucket.org/seanfarley/crecord
fi

if [[ $PACKAGES == *hgsub* ]]; then
  install python http://bitbucket.org/durin42/hgsubversion
fi

if [[ $PACKAGES == *hg-git* ]]; then
  install python http://www.samba.org/~jelmer/dulwich/dulwich-0.8.3.tar.gz
  install python http://bitbucket.org/durin42/hg-git
fi

if [[ $PACKAGES == *global* ]]; then
  install gnu http://tamacom.com/global/global-6.1.tar.gz
fi

if [[ $PACKAGES == bc || $PACKAGES == *bc[!-a-zA-Z]* || $PACKAGES == *bash-comp* || $PACKAGES == *bashcomp* ]]; then
  # install gnu http://bash-completion.alioth.debian.org/files/bash-completion-1.3.tar.bz2
  # after bash-completion is installed, get the mercurial and git scripts
  install script http://selenic.com/hg/raw-file/7ed056f1e97d/contrib/bash_completion $LOC/etc/bash_completion.d/mercurial
  install script https://raw.github.com/gitster/git/master/contrib/completion/git-completion.bash $LOC/etc/bash_completion.d/git
fi

if [[ $PACKAGES == *pss* ]]; then
  install python http://pypi.python.org/packages/source/p/pss/pss-0.34.tar.gz
fi

if [[ $PACKAGES == *emac* ]]; then
  install gnu http://alpha.gnu.org/gnu/emacs/pretest/emacs-24.0.92.tar.gz "--without-x --without-jpeg --without-gif --without-sound"
fi
