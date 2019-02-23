call plug#begin('~/.vim/plugged')
Plug 'ervandew/supertab'
Plug 'scrooloose/nerdtree'
Plug 'majutsushi/tagbar'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'easymotion/vim-easymotion'
Plug 'dyng/ctrlsf.vim'
Plug 'terryma/vim-multiple-cursors'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'Yggdroot/indentLine'
Plug 'tpope/vim-surround'
Plug 'godlygeek/tabular'
Plug 'plasticboy/vim-markdown'
Plug 'jiangmiao/auto-pairs'
Plug 'derekwyatt/vim-scala'
Plug 'amix/open_file_under_cursor.vim'
Plug 'mhinz/vim-startify'
Plug 'Valloric/YouCompleteMe'
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
" nmap <leader>ln :lnext<CR>
" nmap <leader>lp :lprevious<CR>
" nmap <leader>cn :cnext<CR>
" nmap <leader>cp :cprevious<CR>
nmap <C-j> <C-W>j
nmap <C-k> <C-W>k
nmap <C-h> <C-W>h
nmap <C-l> <C-W>l
inoremap <C-E> <End>
inoremap <C-A> <Home>
map <leader>tp :setlocal paste!<cr>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" GENERAL PLUGINS
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" For NERDTree
let NERDTreeIgnore=['\.pyc$', '\~$'] "ignore files in NERDTree
let NERDTreeHighlightCursorline=1
map <F2> :NERDTreeToggle %<CR>

" For tagbar
let g:tagbar_right=1
let g:tagbar_type_markdown = {
        \ 'ctagstype' : 'markdown',
        \ 'kinds' : [
                \ 'h:headings',
        \ ],
    \ 'sort' : 0
\ }
map <F3> :TagbarToggle<CR>

" For ctrlp
let g:ctrlp_map = '<leader>f'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_custom_ignore = {
    \ 'dir':  '\v[\/]\.(git|hg|svn|rvm)$',
    \ 'file': '\v\.(exe|so|dll|zip|tar|tar.gz|pyc|ipynb|class)$',
\ }
nmap <leader>b :CtrlPBuffer<CR>
nmap <leader>t :CtrlPTag<CR>

" For supertab
let g:SuperTabDefaultCompletionType = "<c-p>"
let g:SuperTabContextDefaultCompletionTyper= "<c-p>"

" For EasyMotion
map , <Plug>(easymotion-s)

" For Ctrlsf
nmap     <C-X>f <Plug>CtrlSFPrompt<Space>
vmap     <C-X>f <Plug>CtrlSFVwordPath
vmap     <C-X>F <Plug>CtrlSFVwordExec
nmap     <C-X>n <Plug>CtrlSFCwordPath
nmap     <C-X>p <Plug>CtrlSFPwordPath
nnoremap <C-X>o :CtrlSFOpen<CR>
nnoremap <C-X>t :CtrlSFToggle<CR>
inoremap <C-X>t <Esc>:CtrlSFToggle<CR>
nnoremap <leader>sp :CtrlSF -filetype python<Space>
nnoremap <leader>sc :CtrlSF -filetype cc<Space>
nnoremap <leader>sz :CtrlSF -filetype cpp<Space>
nnoremap <leader>so :CtrlSF -filetype ocaml<Space>
nnoremap <leader>sm :CtrlSF -filetype markdown<Space>
nnoremap <leader>sj :CtrlSF -filetype java<Space>
nnoremap <leader>ss :CtrlSF -filetype scala<Space>
let g:ctrlsf_ignore_dir = ['.git', '.svn', 'tags', 'cscope*.out']
let g:ctrlsf_default_view_mode = 'compact'

" For multiple-cursors
let g:multi_cursor_use_default_mapping=0
let g:multi_cursor_start_key='<F5>'
let g:multi_cursor_next_key='<C-n>'
let g:multi_cursor_prev_key='<C-p>'
let g:multi_cursor_skip_key='<C-x>'
let g:multi_cursor_quit_key='<Esc>'

" For ultisnips
let g:UltiSnipsExpandTrigger="yy"
let g:UltiSnipsListSnippets="yY"
let g:UltiSnipsJumpForwardTrigger="YY"
let g:UltiSnipsJumpBackwardTrigger="OO"

" For indentLine
let g:indentLine_char = '┆'
let g:indentLine_enable = 1

" For auto-pairs
let g:AutoPairsShortcutToggle = '<leader>ta'


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" SPECIFIC PLUGINS & SETTINGS
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" --------------
"  YouCompleteMe
" --------------

autocmd FileType c,cpp,java,python nmap <leader>gd :YcmCompleter GoTo<CR>
autocmd FileType c,cpp,java,python nmap <leader>gc :YcmCompleter GoToReferences<CR>
autocmd FileType c,cpp,java,python nmap <leader>go :YcmCompleter GetDoc<CR>
autocmd FileType java nmap <leader>oi :YcmCompleter OrganizeImports<CR>
let g:ycm_min_num_of_chars_for_completion = 3
let g:ycm_add_preview_to_completeopt = 0

" -------
" C & C++
" -------

autocmd FileType cpp setlocal ts=4 sts=4 et sw=4
" autocmd FileType c,cpp nmap <leader>gc :cs find c <cword><CR>
" autocmd FileType c,cpp nmap <leader>gd :cs find g <cword><CR>
autocmd FileType c,cpp map <F3> :TagbarToggle<CR>

" ------------
" Java & Scala
" ------------

autocmd FileType java setlocal ts=4 sts=4 et sw=4
autocmd FileType scala setlocal ts=2 sts=2 et sw=2
" autocmd FileType java nmap <leader>gc :cs find c <cword><CR>
" autocmd FileType java nmap <leader>gd :cs find g <cword><CR>
autocmd FileType java,scala map <F3> :TagbarToggle<CR>

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

" For vim-markdown
let g:vim_markdown_folding_disabled = 1
" let g:vim_markdown_folding_style_pythonic = 0
let g:vim_markdown_conceal = 0
let g:vim_markdown_toc_autofit = 1
let g:vim_markdown_frontmatter = 1
let g:vim_markdown_emphasis_multiline = 1
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
