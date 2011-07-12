"~/.vimrc
"=======================================================================
"          FILE:  Configuration file for GVim.
" 
"         USAGE:  To be copied in your home.
" 
"   DESCRIPTION:  It will load automatically all settings put in different
"   files. These files are located in ~/.vim/vimrc/ Change or copy it according
"   to your needs.
" 
"       OPTIONS:  ---
"  REQUIREMENTS:  you should have GVim >= 7.2
"          BUGS:  ---
"         NOTES:  In case of problem, see here : http://alexkrispin.wordpress.com/
"        AUTHOR: Alexandre Krispin, k.m.alexandre@gmail.com
"       COMPANY:  ---
"       CREATED:  I don't remember the date...
"      REVISION:   2010年 10月  9日 土曜日 01:35:48 CEST
"=======================================================================

" Use pathogen to easily modify the runtime path to include all
" plugins under the ~/.vim/bundle directory

" This source imports all of my general settings. Indent, hilight, etc.
source ~/.vim/vimrc/vimrc_general

"This source imports all of my miscellanous settings. Tab bar, etc
source ~/.vim/vimrc/vimrc_miscellaneous

" This source imports all of my plugin settings and bindings.
source ~/.vim/vimrc/vimrc_mapping_and_plugins

" My own personal settings
call pathogen#helptags()
call pathogen#runtime_append_all_bundles()

set nobackup
set noswapfile

nnoremap ; :
nnoremap j gj
nnoremap k gk

" Easy window navigation
" map <C-h> <C-w>h
" map <C-j> <C-w>j
" map <C-k> <C-w>k
" map <C-l> <C-w>l

" Clears the search highlight
nmap <silent> <leader>/ :nohlsearch<CR>

" Use Q for formatting the current paragraph (or selection)
vmap Q gq
nmap Q gqap

"---------------------------------------------------------------------
"                        spell checking
"---------------------------------------------------------------------
function! s:spell_en()
    if !exists("s:spell_check") || s:spell_check == 0
        echo "Spell checking activated (english)"
        let s:spell_check = 1
        setlocal spell spelllang=en
    else
        echo "Spell checking canceled"
        let s:spell_check = 0
        setlocal spell spelllang=
    endif
endfunction

"--------------------------
"To enable spell checking for English:
"--------------------------
noremap  <F12>  :call <SID>spell_en()<CR>
inoremap <F12>  :call <SID>spell_en()<CR>
vnoremap <F12>  :call <SID>spell_en()<CR>

" --------------------------
"  Mercurial Settings
" --------------------------
nmap <Leader>hga <Plug>VCSAdd
nmap <Leader>hgn <Plug>VCSAnnotate
nmap <Leader>hgN <Plug>VCSAnnotate!
nmap <Leader>hgc <Plug>VCSCommit
nmap <Leader>hgD <Plug>VCSDelete
" nmap <Leader>hgd <Plug>VCSDiff
nmap <Leader>hgg <Plug>VCSGotoOriginal
nmap <Leader>hgG <Plug>VCSGotoOriginal!
nmap <Leader>hgi <Plug>VCSInfo
nmap <Leader>hgl <Plug>VCSLog
nmap <Leader>hgL <Plug>VCSLock
nmap <Leader>hgr <Plug>VCSReview
nmap <Leader>hgs <Plug>VCSStatus
nmap <Leader>hgu <Plug>VCSUpdate
nmap <Leader>hgU <Plug>VCSUnlock
" nmap <Leader>hgv <Plug>VCSVimDiff
nmap <Leader>hgd <Plug>VCSVimDiff
nmap <Leader>hgv :<C-u>!hg vdiff<CR>


" ---------------------
"  Clang settings
" ---------------------
" let g:clang_exec="/opt/local/bin/clang"
let g:clang_use_library=1
let g:clang_library_path='/Developer/usr/clang-ide/lib'
let g:clang_snippets=1
let g:clang_user_options = '-I/opt/local/include -I$PETSC_DIR/include -I$PETSC_DIR/$PETSC_ARCH/include'

" ---------------------
" Set the tab width and softtabstop
" ---------------------
set expandtab
set shiftwidth=2
set softtabstop=2

" --------------------
" Keyboard shortcuts for buffers
" ---------------------
map <C-h> :<C-u>bp<CR>
map <C-l> :<C-u>bn<CR>
map <C-j> :<C-u>cclose<CR>
map <C-k> :<C-u>copen<CR>
map <leader>d :<C-u>bd<CR>

map <C-n> :<C-u>cn<CR>
map <C-p> :<C-u>cp<CR>

" --------------------
" Keyboard shortcuts for Gtags
" ---------------------
map <C-]> :GtagsCursor<CR>

" Open the Gtags output window.  Set this variable to zero, to not open
" the Gtags output window by default.  You can open it manually by using
" the :cwindow command.
let Gtags_OpenQuickfixWindow = 0

" ---------------------
" Ignore file filters
" --------------------
let NERDTreeIgnore=['\.vim$', '\~$', 'tags', 'CTAGS', 'ETAGS', 'TAGS', '\.d', '\.o', '\.gz', '\.out', '\.eps', '\.pdf', '\.vrb', '\.nav', '\.snm', '\.gnuplot', '\.toc', '\.table', '\.aux', '\.pdfsync']
let g:fuf_file_exclude = '\v\~$|tags|[C|E]TAGS|TAGS|\.(o|d|exe|dll|bak|swp|table|toc|gnuplot|snm|nav|vrb|pdf|eps|out|gz|jpg|png|svg|aux|pdfsync)$|.*\.dSYM($|[/\\])|(^|[/\\])\.(hg|git|bzr|builds)($|[/\\])|arch-.*$|RDict\..*$'

" --------------------
" Map z+<Space> to toggle a fold
" --------------------
map z<Space> za

" --------------------
"  Nice colorscheme
" --------------------
" colorscheme jellybeans
" colors jellybeans
colorscheme xoria256
colors xoria256

" --------------------
" LaTeX settings
" --------------------
let tlist_tex_settings = 'latex;l:labels;s:sections;t:subsections;u:subsubsections'
let g:tex_flavor = 'latex'
let g:Tex_MultipleCompileFormats='pdf'
set grepprg=grep\ -nH\ $*
" Commenting these out because I can't get them to work right
" map <C-k> <Plug>IMAP_JumpBack
" imap <C-k> <Plug>IMAP_JumpBack
" map <C-L> <Plug>Tex_LeftRight
" imap <C-L> <Plug>Tex_LeftRight
" map <C-i> <Plug>Tex_InsertItemOnThisLine.
" imap <C-i> <Plug>Tex_InsertItemOnThisLine.
" map <C-l> <F5>
" imap <C-l> <F5>
map ]s :/\\\(sub\)\{,2}section\s*{ :noh
map [s  \\\(sub\)\{,2}section\s*{ :noh
let g:Tex_FoldedEnvironments = 'solution,subproblem,problem'

if has('macunix')
        let g:Tex_ViewRule_pdf = 'Skim'
        let g:Tex_CompileRule_pdf = 'xelatex -shell-escape --synctex=1 --interaction=nonstopmode $*'
endif

" --------------------
" add additional tags here or comment out not-used ones
" Note: to get ctags highlighting to work, must edit
" ctags_highlighting.vim
" --------------------
set tags+=$HOME/projects/petsc/petsc-dev/CTAGS
" set tags+=$HOME/projects/petsc/petsc-3.1/tags

" --------------------
" Auto-save when focus is lost
" --------------------
:au FocusLost * :wa
:set autowriteall

" --------------------
"  Set NERDCommenter settings
" --------------------
let NERDCommentWholeLinesInVMode=1
let NERDSpaceDelims=1

" --------------------
"  Shortcuts for FuzzyFinder
" --------------------
map <leader>f :<C-u>FufFile **/<CR>
map <leader>t :<C-u>FufTag<CR>
map <leader>b :<C-u>FufBufferTag<CR>
map <leader>B :<C-u>FufBuffer<CR>
map <leader>q :<C-u>FufQuickfix<CR>

" --------------------
"  Forgot why I needed this setting
" --------------------
set nocp " non vi compatible mode
 
" --------------------
" Custom commands to open projects
" --------------------
silent! :command BOUT cd ~/projects/bout/bout-dev
silent! :command PETSC cd ~/projects/petsc/petsc-dev
silent! :command TALKS cd ~/projects/talks
silent! :command SP cd ~/projects/scienceports
