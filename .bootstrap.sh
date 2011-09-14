#!/bin/sh

REPO="settings"
FILES=".aliases .bash_profile .bash_prompt .bootstrap.sh .ctags .dir_colors .emacs .exports .functions .gdbinit .globalrc .hgignore .hgrc .hgsub .hgsubstate .inputrc .osx .profile .vimrc"
DIRS=".emacs.d .ssh .vim .hg"

for file in $FILES; do mv $REPO/$file .; done
for dir in $DIRS; do
	if [[ -d $dir ]]; then
		mv $dir backup-$dir
	fi
	mv $REPO/$dir .
done
