call plug#begin('~/.vim/plugged')
Plug 'ervandew/supertab'
Plug 'scrooloose/nerdtree'
Plug 'majutsushi/tagbar'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'easymotion/vim-easymotion'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'mileszs/ack.vim'
" Plug 'Yggdroot/indentLine'
Plug 'tpope/vim-surround'
Plug 'godlygeek/tabular'
Plug 'plasticboy/vim-markdown'
Plug 'amix/open_file_under_cursor.vim'
Plug 'mhinz/vim-startify'
" Plug 'davidhalter/jedi-vim'
Plug 'jiangmiao/auto-pairs'
" Plug 'Valloric/YouCompleteMe'
Plug 'davidoc/taskpaper.vim'
Plug 'tommcdo/vim-kangaroo'
call plug#end()


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" BASIC SETTINGS
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

syntax on
set nu
set ruler
set ai
set hlsearch
set noet
set ts=8
set mouse=a
" set cc=81
set ls=2
set conceallevel=0
set wildmenu
set completeopt-=preview

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
nmap <C-j> <C-W>j
nmap <C-k> <C-W>k
nmap <C-h> <C-W>h
nmap <C-l> <C-W>l
nmap <leader>q :b#<bar>bd#<CR>
inoremap <C-E> <End>
inoremap <C-A> <Home>
inoremap <C-F> <Right>
inoremap <C-B> <Left>
nmap <leader>pt :setlocal paste!<cr>


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

" For ctrlp
let g:ctrlp_map = '<leader>f'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_custom_ignore = {
    \ 'dir':  '\v[\/]\.(git|hg|svn|rvm)$',
    \ 'file': '\v\.(exe|so|dll|zip|tar|tar.gz|pyc|ipynb|class|o)$',
\ }
let g:ctrlp_extensions = ['buffertag', 'bookmarkdir']
let g:ctrlp_key_loop = 1
let g:ctrlp_max_files = 0
let g:ctrlp_max_depth=40
let g:ctrlp_by_filename = 1
let g:ctrlp_working_path_mode = 'a'
let g:ctrlp_cache_dir = $HOME . '/.cache/ctrlp'
if executable('ag')
	let g:ctrlp_user_command = 'ag %s -i --nocolor --nogroup --hidden -g ""'
endif
nmap <leader>b :CtrlPBuffer<CR>
nmap <leader>ji :CtrlPBufTag<CR>
nmap <leader>jt :CtrlPTag<CR>
" nmap <leader>t :CtrlPTag<CR>

" For supertab
let g:SuperTabDefaultCompletionType = "<c-p>"
let g:SuperTabContextDefaultCompletionTyper= "<c-p>"

" For EasyMotion
nmap , <Plug>(easymotion-s)

" For Ack.vim
if executable('ag')
	let g:ackprg = 'ag --vimgrep'
endif
" set shellpipe=>  " 解决 ag 的输出结果重定向至 stdout，但可能导致其他插件问题
nnoremap <leader>ss :Ack!<Space>
nnoremap <leader>sp :Ack! --python<Space>
nnoremap <leader>sc :Ack! --cc<Space>
nnoremap <leader>sz :Ack! --cpp<Space>
nnoremap <leader>so :Ack! --ocaml<Space>
nnoremap <leader>sm :Ack! --markdown<Space>
nnoremap <leader>sj :Ack! --java<Space>

" For ultisnips
let g:UltiSnipsExpandTrigger = "yy"
let g:UltiSnipsListSnippets = "yY"
let g:UltiSnipsJumpForwardTrigger = "YY"
let g:UltiSnipsJumpBackwardTrigger = "OO"
let g:UltiSnipsSnippetDirectories = ['~/.vim/snippets/']
let g:UltiSnipsSnippetsDir = "~/.vim/snippets/"

" " For indentLine
" let g:indentLine_char = '┆'
" let g:indentLine_enable = 1

" For auto-pairs
let g:AutoPairsShortcutToggle = '<leader>at'


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" SPECIFIC PLUGINS & SETTINGS
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" --------------
"  YouCompleteMe
" --------------

" autocmd FileType c,cpp,java,python nmap <leader>gd :YcmCompleter GoTo<CR>
" autocmd FileType c,cpp,java,python nmap <leader>gc :YcmCompleter GoToReferences<CR>
" autocmd FileType c,cpp,java,python nmap <leader>go :YcmCompleter GetDoc<CR>
" autocmd FileType java nmap <leader>oi :YcmCompleter OrganizeImports<CR>
" let g:ycm_min_num_of_chars_for_completion = 3
" let g:ycm_add_preview_to_completeopt = 0

" -------
" C & C++
" -------

autocmd FileType cpp setlocal ts=4 sts=4 et sw=4
autocmd FileType c,cpp nmap <leader>gc :cs find c <cword><CR>
autocmd FileType c,cpp nmap <leader>gd :cs find g <cword><CR>
autocmd FileType c,cpp map <F3> :TagbarToggle<CR>

" ----
" Java
" ----

autocmd FileType java setlocal ts=4 sts=4 et sw=4
autocmd FileType java nmap <leader>gc :cs find c <cword><CR>
autocmd FileType java nmap <leader>gd :cs find g <cword><CR>
autocmd FileType java map <F3> :TagbarToggle<CR>

" ------
" Python
" ------

autocmd FileType python setlocal ts=4 sts=4 et sw=4
autocmd FileType python map <F3> :TagbarToggle<CR>

" " Add the virtualenv's site-packages to vim path
" if has('python')
" py << EOF
" import os.path
" import sys
" import vim
" if 'VIRTUAL_ENV' in os.environ:
"     project_base_dir = os.environ['VIRTUAL_ENV']
"     sys.path.insert(0, project_base_dir)
"     activate_this = os.path.join(project_base_dir, 'bin/activate_this.py')
"     execfile(activate_this, dict(__file__=activate_this))
" EOF
" endif

" " For jedi
" let g:jedi#completions_enabled = 1
" autocmd FileType python setlocal completeopt-=preview
" " let g:jedi#auto_initialization = 1
" " let g:jedi#auto_vim_configuration = 0
" " let g:jedi#use_tabs_not_buffers = 0
" " let g:jedi#use_splits_not_buffers = "left"
" let g:jedi#popup_on_dot = 0
" let g:jedi#popup_select_first = 1
" let g:jedi#show_call_signatures = "2" 	" Set to 2 in command line
" let g:jedi#goto_command = "<leader>gd"
" let g:jedi#goto_assignments_command = "<leader>pa"
" let g:jedi#documentation_command = "K"
" let g:jedi#usages_command = "<leader>gc"
" let g:jedi#completions_command = "<C-Space>"
" let g:jedi#rename_command = "<leader>pr"

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
autocmd FileType markdown setlocal ts=4 sts=4 et sw=4 conceallevel=0

" For vim-markdown
let g:vim_markdown_folding_disabled = 1
" let g:vim_markdown_folding_style_pythonic = 0
let g:vim_markdown_conceal = 0
let g:vim_markdown_toc_autofit = 1
let g:vim_markdown_frontmatter = 1
let g:vim_markdown_strikethrough = 1
let g:vim_markdown_emphasis_multiline = 1
let g:vim_markdown_toc_is_open = 0
" function! MarkdownTocToggle()
" 	if g:vim_markdown_toc_is_open
" 		lclose
" 		let g:vim_markdown_toc_is_open = 0
" 	else
" 		Toc
" 		let g:vim_markdown_toc_is_open = 1
" 	endif
" endfunction
" autocmd FileType markdown map <F3> :call MarkdownTocToggle()<CR>
