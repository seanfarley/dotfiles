" Preamble -------------------------------------------------------------------- {{{

  " Need to set this before vim-latex gets loaded
  imap <C-n> <Plug>IMAP_JumpForward
  nmap <C-n> <Plug>IMAP_JumpForward
  vmap <C-n> <Plug>IMAP_JumpForward

  filetype off
  call pathogen#helptags()
  call pathogen#runtime_append_all_bundles()
  filetype plugin indent on
  set nocompatible
  syntax on

" }}}

" Import all specific settings ------------------------------------------------ {{{

  source ~/.vim/vimrc/functions
  source ~/.vim/vimrc/general
  source ~/.vim/vimrc/mappings
  source ~/.vim/vimrc/plugins
  source ~/.vim/vimrc/filetypes
  source ~/.vim/vimrc/macvim

" }}}

" Custom commands to open projects -------------------------------------------- {{{

  silent! :command BOUT cd ~/projects/bout/bout-dev
  silent! :command PETSC cd ~/projects/petsc/petsc-dev
  silent! :command TALKS cd ~/projects/talks
  silent! :command SP cd ~/projects/scienceports
  silent! :command RESEARCH cd ~/projects/research
  silent! :command HW cd ~/projects/class/2011-fall
  silent! :command CV cd ~/projects/cvitae
  silent! :command MC cd ~/projects/mercurial

" }}}
