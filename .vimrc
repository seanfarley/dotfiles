syntax on
set expandtab
set shiftwidth=2
set softtabstop=2
set smartindent
set foldmethod=syntax
"set ignorecase

" NERDTree filters
let NERDTreeIgnore=['\.vim$', '\~$', 'tags', 'CTAGS', 'ETAGS', 'TAGS', '\.d', '\.o', '\.gz', '\.out', '\.eps', '\.pdf', '\.vrb', '\.nav', '\.snm', '\.gnuplot', '\.toc', '\.table', '\.aux', '\.pdfsync']
let g:fuf_file_exclude = '\v\~$|tags|[C|E]TAGS|\.(o|d|exe|dll|bak|swp|table|toc|gnuplot|snm|nav|vrb|pdf|eps|out|gz|jpg|png|svg|aux|pdfsync)$|.*\.dSYM($|[/\\])|(^|[/\\])\.(hg|git|bzr|builds)($|[/\\])'

map z<Space> za

" automatically open and close the popup menu / preview window
au CursorMovedI,InsertLeave * if pumvisible() == 0|silent! pclose|endif
set completeopt=menuone,menu,longest,preview

" configure tags - add additional tags here or comment out not-used ones
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
map <leader>b :<C-u>FufBufferTag<CR>
map <leader>B :<C-u>FufBuffer<CR>
map <leader>q :<C-u>FufQuickfix<CR>

" LaTeX support for ctags
let tlist_tex_settings = 'latex;l:labels;s:sections;t:subsections;u:subsubsections'

if has('macunix')
        let g:Tex_ViewRule_pdf = 'Skim'
        let g:Tex_CompileRule_pdf = 'pdflatex -shell-escape -synctex=1 --interaction=nonstopmode $*'
endif

set nocp " non vi compatible mode

" Custom commands to open projects
:command BOUT cd ~/projects/bout/bout++-dev
:command PETSC cd ~/projects/petsc/petsc-dev
:command TALKS cd ~/projects/talks
