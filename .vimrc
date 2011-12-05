" Preamble -------------------------------------------------------------------- {{{

  " Need all of these to be set before plugins are loaded
  imap <C-n> <Plug>IMAP_JumpForward
  nmap <C-n> <Plug>IMAP_JumpForward
  vmap <C-n> <Plug>IMAP_JumpForward

  let GtagsCscope_Auto_Load = 1
  let g:GtagsCscope_Ignore_InitErrors = 1
  let g:Gtags_Ignore_InitErrors = 1
  let g:unite_data_directory = expand('~/.vim/tmp/unite/')
  let g:vimshell_temporary_directory = expand('~/.vim/tmp/vimshell')
  let g:fuf_dataDir = expand('~/.vim/tmp/fuf-data')
  let g:vimfiler_data_directory = expand('~/.vim/tmp/vimfiler')

  runtime bundle/pathogen/autoload/pathogen.vim
  call pathogen#infect()
  syntax on
  filetype plugin indent on

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
