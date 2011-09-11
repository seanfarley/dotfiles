"~/.vimrc
"=======================================================================
"          FILE:  Configuration file for vim.
" 
"         USAGE:  To be copied in your home.
" 
"   DESCRIPTION:  It will load automatically all settings put in different
"   files. These files are located in ~/.vim/vimrc/ Change or copy it according
"   to your needs.
" 
"       OPTIONS:  ---
"  REQUIREMENTS:  you should have vim >= 7.3
"          BUGS:  ---
"         NOTES:  In case of problem, see here : http://alexkrispin.wordpress.com/
"        AUTHOR:  Alexandre Krispin, k.m.alexandre@gmail.com
"        AUTHOR:  Sean Farley, sean.michael.farley@gmail.com
"       COMPANY:  ---
"      REVISION:  2011, Sept. 10 8:04pm
"=======================================================================

" Import all specific settings
source ~/.vim/vimrc/general
source ~/.vim/vimrc/functions
source ~/.vim/vimrc/mappings

" --------------------
" Custom commands to open projects
" --------------------
silent! :command BOUT cd ~/projects/bout/bout-dev
silent! :command PETSC cd ~/projects/petsc/petsc-dev
silent! :command TALKS cd ~/projects/talks
silent! :command SP cd ~/projects/scienceports
silent! :command RESEARCH cd ~/projects/research
