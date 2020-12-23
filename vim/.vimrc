call plug#begin('~/.vim/plugged')
Plug 'ervandew/supertab'
Plug 'scrooloose/nerdtree'
Plug 'majutsushi/tagbar'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'mileszs/ack.vim'
Plug 'tpope/vim-surround'
Plug 'godlygeek/tabular'
Plug 'amix/open_file_under_cursor.vim'
Plug 'topin27/taskpaper.vim'
Plug 'topin27/JavaImp.vim'
Plug 'junegunn/fzf', {'dir': '~/bins/fzf', 'do': './install --bin'}
Plug 'junegunn/fzf.vim'
Plug 'pangloss/vim-javascript'
Plug 'tpope/vim-fugitive'
Plug 'vim-pandoc/vim-pandoc-syntax'
Plug 'aklt/plantuml-syntax'
call plug#end()


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" BASIC SETTINGS
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

filetype plugin on

syntax on
syntax enable
set nu
set rnu
set is
set ruler
set ai
set hlsearch
set noet
set ts=8
set mouse=
set ls=2
set conceallevel=2
set wildmenu
set completeopt=menuone
" set iskeyword+=-
set nocompatible

let maplocalleader = ','

cnoremap <C-G> <C-F>
cnoremap <C-A> <Home>
cnoremap <C-E> <End>
cnoremap <C-F> <Right>
cnoremap <C-B> <Left>
cnoremap <C-P> <Up>
cnoremap <C-N> <Down>

nmap <TAB> <C-w>w
nmap <C-j> <C-W>j
nmap <C-k> <C-W>k
nmap <C-h> <C-W>h
nmap <C-l> <C-W>l
nmap <C-Down> <C-W>j
nmap <C-Up> <C-W>k
nmap <C-Left> <C-W>h
nmap <C-Right> <C-W>l
nmap <LocalLeader>q :b#<bar>bd#<CR><C-W><C-P>
nmap QQ :q<CR><C-W><C-P>
nmap <Leader>tp :setlocal paste!<cr>
nmap <Leader>tc :exec &conceallevel ? "set conceallevel=0" : "set conceallevel=2"<CR>
" nmap <LocalLeader>w :w<CR>
nmap s :w<CR>
nmap <F5> :w<CR> :!<Up><CR>
vmap <Leader>pw :w! ~/.vim/clipboard.txt
nmap <Leader>pr :r ~/.vim/clipboard.txt

inoremap <C-E> <End>
inoremap <C-A> <Home>
inoremap <C-F> <Right>
inoremap <C-B> <Left>

nnoremap g] g<C-]>

autocmd FileType qf nnoremap <silent><buffer> q :q<cr>

if !exists('g:my_lasttab')
  let g:my_lasttab = 1
endif
nmap <Leader>tt :exe "tabn ".g:my_lasttab<CR>
au TabLeave * let g:my_lasttab = tabpagenr()

packadd! matchit


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" GENERAL PLUGINS
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" For NERDTree
let g:NERDTreeIgnore = ['\.pyc$', '\~$'] "ignore files in NERDTree
let g:NERDTreeHighlightCursorline = 1
map <F2> :NERDTreeToggle<CR>

" For tagbar
let g:tagbar_right = 1
let g:tagbar_autofocus = 1
let g:tagbar_type_markdown = {
	\ 'ctagstype' : 'markdown',
	\ 'kinds' : [
		\ 'h:headings',
		\ 'f:footnotes',
	\ ],
	\ 'sort' : 0
\ }
let g:tagbar_type_taskpaper = {
	\ 'ctagstype' : 'taskpaper',
	\ 'kinds' : [
		\ 'p:projects',
	\ ],
	\ 'sort' : 0
\ }
map <F3> :TagbarToggle<CR>

" For fzf
let g:fzf_command_prefix = "Fzf"
nnoremap <silent> <Leader>f :FzfFiles<CR>
nnoremap <silent> <Leader>b :FzfBuffers<CR>
nnoremap <silent> <Leader>w :FzfWindows<CR>
nnoremap <silent> <Leader>m :FzfMarks<CR>
nnoremap <silent> <Leader>jt :FzfTags<CR>
nnoremap <silent> <Leader>ji :FzfBTags<CR>
nnoremap <silent> <Leader>cs :FzfSnippets<CR>
let g:fzf_action = {
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-x': 'split',
  \ 'ctrl-v': 'vsplit' }
let g:fzf_layout = { 'down': '~40%' }

" For supertab
let g:SuperTabDefaultCompletionType = "<c-x><c-p>"
let g:SuperTabRetainCompletionDuration = 'insert'
let g:SuperTabLongestEnhanced = 1

" For Ack.vim
if executable('ag')
	let g:ackprg = 'ag --vimgrep'
endif
" set shellpipe=>  " 解决 ag 的输出结果重定向至 stdout，但可能导致其他插件问题
nnoremap <Leader>ss :Ack! "" -sw<Left><Left><Left><Left><Left>
nnoremap <Leader>s/ :Ack! "" %<Left><Left><Left>
autocmd FileType python nnoremap <LocalLeader>ss :Ack! --python "" -sw<Left><Left><Left><Left><Left>
autocmd FileType c  nnoremap <LocalLeader>ss :Ack! --cc "" -sw<Left><Left><Left><Left><Left>
autocmd FileType cpp  nnoremap <LocalLeader>ss :Ack! --cpp "" -sw<Left><Left><Left><Left><Left>
autocmd FileType ocaml  nnoremap <LocalLeader>ss :Ack! --ocaml "" -sw<Left><Left><Left><Left><Left>
autocmd FileType markdown  nnoremap <LocalLeader>ss :Ack! --markdown "" -sw<Left><Left><Left><Left><Left>
autocmd FileType java  nnoremap <LocalLeader>ss :Ack! --java "" -sw<Left><Left><Left><Left><Left>
autocmd FileType javascript  nnoremap <LocalLeader>ss :Ack! --js "" -sw<Left><Left><Left><Left><Left>
autocmd FileType makefile nnoremap <LocalLeader>ss :Ack! --make "" -sw<Left><Left><Left><Left><Left>
nnoremap <Leader>sp :Ack! --python "" -sw<Left><Left><Left><Left><Left>
nnoremap <Leader>sc :Ack! --cc "" -sw<Left><Left><Left><Left><Left>
nnoremap <Leader>sz :Ack! --cpp "" -sw<Left><Left><Left><Left><Left>
nnoremap <Leader>so :Ack! --ocaml "" -sw<Left><Left><Left><Left><Left>
nnoremap <Leader>sm :Ack! --markdown "" -sw<Left><Left><Left><Left><Left>
nnoremap <Leader>sj :Ack! --java "" -sw<Left><Left><Left><Left><Left>
nnoremap <Leader>sa :Ack! --js "" -sw<Left><Left><Left><Left><Left>
nnoremap <Leader>sk :Ack! --make "" -sw<Left><Left><Left><Left><Left>

" For ultisnips
" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger = "<C-d>"
let g:UltiSnipsJumpForwardTrigger = "<C-f>"
let g:UltiSnipsJumpBackwardTrigger = "<C-b>"
" let g:UltiSnipsListSnippets = "<C-l>"
let g:UltiSnipsSnippetDirectories = ["UltiSnips", "code_snippets"]
let g:UltiSnipsSnippetsDir = "~/.vim/UltiSnips/"

" For Fugitive
nnoremap <Leader>gs :Gstatus<CR>
nnoremap <Leader>gd :Gdiff<CR>
nnoremap <Leader>gc :Gcommit<CR>
nnoremap <Leader>gb :Gblame<CR>
nnoremap <Leader>gl :Glog<CR>
nnoremap <Leader>gp :Git push<CR>
nnoremap <Leader>gw :Gwrite<CR>
nnoremap <Leader>gg :Git<Space>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" SPECIFIC PLUGINS & SETTINGS
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" ---------
" Taskpaper
" ---------
autocmd FileType taskpaper setlocal ts=4 sts=4 et sw=4
autocmd FileType taskpaper nmap <buffer> <LocalLeader>t <Plug>TaskPaperToggleTodo
autocmd FileType taskpaper nmap <buffer> <LocalLeader>s <Plug>TaskPaperToggleStart
autocmd FileType taskpaper nmap <buffer> <LocalLeader>d <Plug>TaskPaperToggleDone
autocmd FileType taskpaper nmap <buffer> <LocalLeader>c <Plug>TaskPaperToggleCancelled

" -------
" C & C++
" -------

autocmd FileType cpp setlocal ts=2 sts=2 et sw=2

" ----
" Java
" ----

autocmd FileType java setlocal ts=4 sts=4 et sw=4

" For JavaImp
let g:JavaImpPaths =
	\ $HOME . "/.vim/JavaImp/jmplst/"
let g:JavaImpDataDir = $HOME . "/.vim/JavaImp"

" ------
" Python
" ------

autocmd FileType python setlocal ts=4 sts=4 et sw=4

" -----
" OCaml
" -----

" " For ocaml
" let g:opamshare = substitute(system('opam config var share'),'\n$','','''')
" execute "set rtp+=" . g:opamshare . "/merlin/vim"
" autocmd FileType ocaml nmap <leader>gd :MerlinLocate<CR>
" autocmd FileType ocaml map <F3> :MerlinOutline<CR>

" ---------------
" Markdown.pandoc
" ---------------

augroup pandoc_syntax
    au! BufNewFile,BufFilePre,BufRead *.md set filetype=markdown.pandoc
augroup END

" autocmd BufNewFile,BufReadPost *.md set filetype=markdown
autocmd FileType markdown.pandoc setlocal ts=4 sts=4 et sw=4 conceallevel=0 formatoptions+=mM
autocmd FileType markdown.pandoc nmap <LocalLeader>ih :s/^#\([# ]\+\)/##\1/c<Home>
autocmd FileType markdown.pandoc nmap <LocalLeader>dh :s/^##\([# ]\+\)/#\1/c<Home>

" ----------
" JavaScript
" ----------
autocmd FileType javascript setlocal ts=2 sts=2 et sw=2

" --------
" Plantuml
" --------
let g:plantuml_executable_script = 0
