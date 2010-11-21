syntax on
set expandtab
set shiftwidth=2
set softtabstop=2
set smartindent
set foldmethod=syntax
"set ignorecase

" NERDTree filters
let NERDTreeIgnore=['\.vim$', '\~$', 'tags', 'CTAGS', 'ETAGS', 'TAGS', '\.d', '\.o']
let g:fuf_file_exclude = '\v\~$|tags|[C|E]TAGS|\.(o|d|exe|dll|bak|swp)$|.*\.dSYM($|[/\\])|(^|[/\\])\.(hg|git|bzr)($|[/\\])'

" build tags of your own project with Ctrl-F12
map <leader>b :<C-u>!ctags -R --sort=yes --c++-kinds=+plx --fields=+iaS --extra=+q .<CR><CR>
map z<Space> za

" OmniCppComplete
let OmniCpp_NamespaceSearch = 2
let OmniCpp_GlobalScopeSearch = 1
let OmniCpp_ShowAccess = 1
let OmniCpp_ShowPrototypeInAbbr = 1 " show function parameters
let OmniCpp_MayCompleteDot = 1 " autocomplete after .
let OmniCpp_MayCompleteArrow = 1 " autocomplete after ->
let OmniCpp_MayCompleteScope = 1 " autocomplete after ::
let OmniCpp_SelectFirstItem = 2 " select first item (but don't insert)
let OmniCpp_DefaultNamespaces = ["std", "_GLIBCXX_STD"]

" Make gccsense use the omni complete
" let g:gccsenseUseOmniFunc = 1

" automatically open and close the popup menu / preview window
au CursorMovedI,InsertLeave * if pumvisible() == 0|silent! pclose|endif
set completeopt=menuone,menu,longest,preview

" configure tags - add additional tags here or comment out not-used ones
set tags+=~/.vim/tags/cpp
"set tags+=$HOME/projects/petsc/petsc-dev/CTAGS
set tags+=$HOME/projects/petsc/petsc-3.1/tags

" Auto-save when focus is lost
:au FocusLost * :wa
:set autowriteall

" Needed for many plugins
filetype plugin on

let NERDCommentWholeLinesInVMode=1
let NERDSpaceDelims=1

map <leader>f :<C-u>FufFile **/<CR>
map <leader>t :<C-u>FufTag<CR>

" LaTeX support for ctags
let tlist_tex_settings = 'latex;l:labels;s:sections;t:subsections;u:subsubsections'

if has('macunix')
	let Tex_DefaultTargetFormat = 'pdf'
	let Tex_ViewRuleComplete_pdf = 'open $*.pdf'	
	let TreatMacViewerAsUNIX = 1
endif

" Smart VS-type <tab-completion>
function! CompleteTab(direction)
  let prec = strpart( getline('.'), 0, col('.')-1 )
  if prec =~ '^\s*$'
    if "backward" == a:direction
      return "\<bs>"
    else
      return "\<tab>"
    endif
  endif

  if exists('&omnifunc') && &omnifunc == 'omni#cpp#complete#Main' && prec =~ '[\.>]\s*[~]\?[a-zA-Z_]*[(]\?$'
    " Class completion... use normal direction
    " Use this with omniCompletion
    if "backward" == a:direction
      return "\<c-p>"
    else
      return "\<c-n>"
    endif
  endif

  " else use generic completion: last-seen / reverse-order
  if "backward" == a:direction
    return "\<c-n>"
  else
    return "\<c-p>"
  endif
endfunction

set nocp " non vi compatible mode
inoremap <tab> <c-r>=CompleteTab("forward")<cr>
inoremap <s-tab> <c-r>=CompleteTab("backward")<cr>

" Custom commands to open projects
:command BOUT cd ~/projects/bout/bout++-dev
:command PETSC cd ~/projects/petsc/petsc-dev
