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

" This source imports all of my general settings. Indent, hilight, etc.
source ~/.vim/vimrc/vimrc_general

"This source imports all of my miscellanous settings. Tab bar, etc
source ~/.vim/vimrc/vimrc_miscellaneous

" This source imports all of my plugin settings and bindings.
source ~/.vim/vimrc/vimrc_mapping_and_plugins

set noswapfile

"---------------------------------------------------------------------
"				spell checking
"---------------------------------------------------------------------
"to enable spell checking by default, uncomment the following line,
"set spell
" automatic spell checking in your language for .txt et .tex. Replace "fr" by your default
" language, "en" if english :

"augroup filetypedetect
  "au BufNewFile,BufRead *.txt setlocal spell spelllang=fr
  "au BufNewFile,BufRead *.tex setlocal spell spelllang=fr
"augroup END

"------------------------------------
" painless spell checking
" for French, you'll need
" wget http://ftp.vim.org/pub/vim/runtime/spell/fr.utf-8.sug
" wget http://ftp.vim.org/pub/vim/runtime/spell/fr.utf-8.spl
" which you may move into ~/.vim/spell
"-------------------------------------
function s:spell_en()
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

"--------Another trick for spell checking is the following line :
" uncomment if you want to use it, type ",C" if you want to enable it,
" and replace aspell by any other dictionary you use (ispell, hunspell)
" map ,C :w<CR>:!aspell -c %<CR>:e %<CR>"

"--------------------------
" A sweet shortcut for editting vimrc
"--------------------------
",v brings up my .vimrc
",V reloads it -- making all changes active (have to save first)

map <leader>v :sp ~/.vimrc<CR><C-W>_
map <silent> <leader>V :source ~/.vimrc<CR>:filetype detect<CR>:exe ":echo 'vimrc reloaded'"<CR>

" --------------------------
"  Mercurial Settings
" --------------------------
let HGCommandAnnotateParent=1
map <leader>hgfm :<C-u>!hg vdiff

" --------------------------
" Set the tab width and softtabstop
" --------------------------
set expandtab
set shiftwidth=2
set softtabstop=2

" ---------------------
" Ignore file filters
" --------------------
let NERDTreeIgnore=['\.vim$', '\~$', 'tags', 'CTAGS', 'ETAGS', 'TAGS', '\.d', '\.o', '\.gz', '\.out', '\.eps', '\.pdf', '\.vrb', '\.nav', '\.snm', '\.gnuplot', '\.toc', '\.table', '\.aux', '\.pdfsync']
let g:fuf_file_exclude = '\v\~$|tags|[C|E]TAGS|\.(o|d|exe|dll|bak|swp|table|toc|gnuplot|snm|nav|vrb|pdf|eps|out|gz|jpg|png|svg|aux|pdfsync)$|.*\.dSYM($|[/\\])|(^|[/\\])\.(hg|git|bzr|builds)($|[/\\])'

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
map <C-k> <Plug>IMAP_JumpBack
imap <C-k> <Plug>IMAP_JumpBack
map <C-L> <Plug>Tex_LeftRight
imap <C-L> <Plug>Tex_LeftRight
map <C-i> <Plug>Tex_InsertItemOnThisLine.
imap <C-i> <Plug>Tex_InsertItemOnThisLine.
map <C-l> <F5>
imap <C-l> <F5>
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
"set tags+=$HOME/projects/petsc/petsc-dev/CTAGS
set tags+=$HOME/projects/petsc/petsc-3.1/tags

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
:command BOUT cd ~/projects/bout/bout++-dev
:command PETSC cd ~/projects/petsc/petsc-dev
:command TALKS cd ~/projects/talks
