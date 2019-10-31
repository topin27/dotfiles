call plug#begin('~/.vim/plugged')
Plug 'ervandew/supertab'
Plug 'scrooloose/nerdtree'
Plug 'majutsushi/tagbar'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'mileszs/ack.vim'
Plug 'tpope/vim-surround'
Plug 'godlygeek/tabular'
Plug 'plasticboy/vim-markdown'
Plug 'amix/open_file_under_cursor.vim'
Plug 'mhinz/vim-startify'
Plug 'topin27/taskpaper.vim'
Plug 'topin27/JavaImp.vim'
Plug 'skywind3000/vim-preview'
Plug 'junegunn/fzf', {'dir': '~/bins/fzf', 'do': './install --bin'}
Plug 'junegunn/fzf.vim'
" Plug 'zxqfl/tabnine-vim'
Plug 'leafgarland/typescript-vim'
Plug 'rhysd/vim-wasm'
Plug 'pangloss/vim-javascript'
Plug 'tpope/vim-fugitive'
Plug 'tommcdo/vim-kangaroo'
Plug 'easymotion/vim-easymotion'
call plug#end()


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" BASIC SETTINGS
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

syntax on
syntax enable
set nu
set rnu
set ruler
set ai
set hlsearch
set noet
set ts=8
set mouse=a
set ls=2
set conceallevel=2
set wildmenu
set completeopt-=preview

let maplocalleader = ','

cnoremap <C-A> <Home>
cnoremap <C-E> <End>
cnoremap <C-F> <Right>
cnoremap <C-B> <Left>
cnoremap <C-P> <Up>
cnoremap <C-N> <Down>
cnoremap <M-b> <S-Left>
cnoremap <M-f> <S-Right>
cnoremap <M-d> <S-Right><Delete>
cnoremap <C-g> <C-c>
cnoremap <C-K> <C-U>

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
nmap <Leader>tp :setlocal paste!<cr>
nmap <Leader>tc :exec &conceallevel ? "set conceallevel=0" : "set conceallevel=2"<CR>
nmap <LocalLeader>w :w<CR>
nmap <F5> :w<CR> :make<Up><CR>

inoremap <C-E> <End>
inoremap <C-A> <Home>
inoremap <C-F> <Right>
inoremap <C-B> <Left>

nnoremap g] g<C-]>

autocmd FileType qf nnoremap <silent><buffer> q :q<cr>


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
nnoremap <silent> <Leader>jt :FzfTags<CR>
nnoremap <silent> <Leader>ji :FzfBTags<CR>
nnoremap <silent> <Leader>cs :FzfSnippets<CR>
let g:fzf_action = {
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-x': 'split',
  \ 'ctrl-v': 'vsplit' }
let g:fzf_layout = { 'down': '~40%' }

" For supertab
let g:SuperTabDefaultCompletionType = "<c-p>"
let g:SuperTabContextDefaultCompletionTyper= "<c-p>"

" For Ack.vim
if executable('ag')
	let g:ackprg = 'ag --vimgrep'
endif
" set shellpipe=>  " 解决 ag 的输出结果重定向至 stdout，但可能导致其他插件问题
nnoremap <Leader>ss :Ack! ""<Left>
nnoremap <Leader>s/ :Ack! "" %<Left><Left><Left>
autocmd FileType python nnoremap <LocalLeader>ss :Ack! --python ""<Left>
autocmd FileType c  nnoremap <LocalLeader>ss :Ack! --cc ""<Left>
autocmd FileType cpp  nnoremap <LocalLeader>ss :Ack! --cpp ""<Left>
autocmd FileType ocaml  nnoremap <LocalLeader>ss :Ack! --ocaml ""<Left>
autocmd FileType markdown  nnoremap <LocalLeader>ss :Ack! --markdown ""<Left>
autocmd FileType java  nnoremap <LocalLeader>ss :Ack! --java ""<Left>
autocmd FileType javascript  nnoremap <LocalLeader>ss :Ack! --js ""<Left>
nnoremap <Leader>sp :Ack! --python ""<Left>
nnoremap <Leader>sc :Ack! --cc ""<Left>
nnoremap <Leader>sz :Ack! --cpp ""<Left>
nnoremap <Leader>so :Ack! --ocaml ""<Left>
nnoremap <Leader>sm :Ack! --markdown ""<Left>
nnoremap <Leader>sj :Ack! --java ""<Left>
nnoremap <Leader>sa :Ack! --js ""<Left>

" For ultisnips
" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger = "<C-e>"
let g:UltiSnipsJumpForwardTrigger = "<C-f>"
let g:UltiSnipsJumpBackwardTrigger = "<C-b>"
" let g:UltiSnipsListSnippets = "<C-l>"
let g:UltiSnipsSnippetDirectories = [$HOME . '/.vim/plugged/vim-snippets/UltiSnips/']
let g:UltiSnipsSnippetsDir = "~/.vim/snippets/"

" For vim-preview
autocmd FileType qf nnoremap <silent><buffer> p :PreviewQuickfix<cr>
autocmd FileType qf nnoremap <silent><buffer> P :PreviewClose<cr>

" For Fuitive
nnoremap <Leader>gs :Gstatus<CR>
nnoremap <Leader>gd :Gdiff<CR>
nnoremap <Leader>gc :Gcommit<CR>
nnoremap <Leader>gb :Gblame<CR>
nnoremap <Leader>gl :Glog<CR>
nnoremap <Leader>gp :Git push<CR>
nnoremap <Leader>gw :Gwrite<CR>
nnoremap <Leader>gg :Git<Space>

" For EasyMotion
let g:EasyMotion_do_mapping = 0 " Disable default mappings
let g:EasyMotion_smartcase = 1
nmap s <Plug>(easymotion-overwin-f2)
nmap <LocalLeader>/ <Plug>(easymotion-sn)
nmap <LocalLeader>h <Plug>(easymotion-linebackward)
nmap <LocalLeader>l <Plug>(easymotion-lineforward)
nmap <LocalLeader>j <Plug>(easymotion-j)
nmap <LocalLeader>k <Plug>(easymotion-k)

" For kangaroo
function! My_GotoDef(name)
	let l:res = fzf#vim#tags(a:name)
	if !empty(l:res)
		KangarooPush
	endif
	return l:res
endfunction
nnoremap <LocalLeader>gg :call My_GotoDef(expand('<cword>'))<CR>
nnoremap <LocalLeader>gb :KangarooPop<cr>


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
autocmd FileType taskpaper map <F3> :TagbarToggle<CR>

" -------
" C & C++
" -------

autocmd FileType cpp setlocal ts=2 sts=2 et sw=2
autocmd FileType c,cpp map <F3> :TagbarToggle<CR>

" ----
" Java
" ----

autocmd FileType java setlocal ts=4 sts=4 et sw=4
autocmd FileType java map <F3> :TagbarToggle<CR>

" For JavaImp
let g:JavaImpPaths =
	\ $HOME . "/.vim/JavaImp/jmplst/"
let g:JavaImpDataDir = $HOME . "/.vim/JavaImp"

" ------
" Python
" ------

autocmd FileType python setlocal ts=4 sts=4 et sw=4
autocmd FileType python map <F3> :TagbarToggle<CR>

" -----
" OCaml
" -----

" " For ocaml
" let g:opamshare = substitute(system('opam config var share'),'\n$','','''')
" execute "set rtp+=" . g:opamshare . "/merlin/vim"
" autocmd FileType ocaml nmap <leader>gd :MerlinLocate<CR>
" autocmd FileType ocaml map <F3> :MerlinOutline<CR>

" --------
" Markdown
" --------

autocmd BufNewFile,BufReadPost *.md set filetype=markdown
autocmd FileType markdown setlocal ts=4 sts=4 et sw=4 conceallevel=0 formatoptions+=mM
let g:vim_markdown_folding_disabled = 0
let g:vim_markdown_folding_style_pythonic = 1
let g:vim_markdown_folding_level = 6
" let g:vim_markdown_no_default_key_mappings = 1
let g:vim_markdown_toc_autofit = 1
let g:vim_markdown_conceal = 1
let g:tex_conceal = ""
let g:vim_markdown_math = 0
let g:vim_markdown_conceal_code_blocks = 0
let g:vim_markdown_frontmatter = 1
let g:vim_markdown_strikethrough = 1

let g:vim_markdown_toc_is_open = 0
function! MarkdownTocToggle()
	if g:vim_markdown_toc_is_open
		lclose
		let g:vim_markdown_toc_is_open = 0
	else
		Toc
		let g:vim_markdown_toc_is_open = 1
	endif
endfunction
autocmd FileType markdown map <F3> :call MarkdownTocToggle()<CR>
autocmd FileType markdown nmap <LocalLeader>ih :,s/^#\([# ]\+\)/##\1/c<Home>
autocmd FileType markdown nmap <LocalLeader>dh :,s/^##\([# ]\+\)/#\1/c<Home>

" ----------
" JavaScript
" ----------
autocmd FileType javascript setlocal ts=4 sts=4 et sw=4
autocmd FileType javascript map <F3> :TagbarToggle<CR>
