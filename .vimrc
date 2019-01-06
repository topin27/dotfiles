set nocompatible              " be iMproved, required
filetype off                  " required

if has("autocmd")
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

Plugin 'scrooloose/nerdtree'
Plugin 'majutsushi/tagbar'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'easymotion/vim-easymotion'
Plugin 'davidhalter/jedi-vim'
Plugin 'ervandew/supertab'
Plugin 'dyng/ctrlsf.vim'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'
Plugin 'Yggdroot/indentLine'
Plugin 'tpope/vim-surround'
Plugin 'godlygeek/tabular'
Plugin 'plasticboy/vim-markdown'
Plugin 'amix/open_file_under_cursor.vim'
Plugin 'jiangmiao/auto-pairs'
Plugin 'derekwyatt/vim-scala'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

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
" set conceallevel=0
set wildmenu

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
nmap <leader>pc :!tmux send-keys -t .+ C-p C-m<CR><CR>
nmap <C-j> <C-W>j
nmap <C-k> <C-W>k
nmap <C-h> <C-W>h
nmap <C-l> <C-W>l
inoremap <C-E> <End>
inoremap <C-A> <Home>
map <leader>tp :setlocal paste!<cr>
nnoremap <C-g> <C-]>
vnoremap <C-g> <C-]>

" 解决输入法切换问题
set noimdisable
autocmd! InsertLeave * set imdisable|set iminsert=0
autocmd! InsertEnter * set noimdisable|set iminsert=0

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" GENERAL PLUGINS
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" For NERDTree
let NERDTreeIgnore=['\.pyc$', '\~$'] "ignore files in NERDTree
let NERDTreeHighlightCursorline=1
map <F2> :NERDTreeToggle<CR><C-W>h

" For tagbar
let g:tagbar_right=1
let g:tagbar_type_markdown = {
        \ 'ctagstype' : 'markdown',
        \ 'kinds' : [
                \ 'h:headings',
        \ ],
    \ 'sort' : 0
\ }
map <F3> :TagbarToggle<CR><C-W>l

" For ctrlp
let g:ctrlp_map = '<leader>f'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_custom_ignore = {
    \ 'dir':  '\v[\/]\.(git|hg|svn|rvm)$',
    \ 'file': '\v\.(exe|so|dll|zip|tar|tar.gz|pyc|ipynb)$',
\ }
nmap <leader>b :CtrlPBuffer<CR>
nmap <leader>t :CtrlPTag<CR>

" For supertab
" let g:SuperTabDefaultCompletionType = "<c-n>"
" let g:SuperTabContextDefaultCompletionType = "<c-n>"

" For EasyMotion
" map <Leader><Leader>j <Plug>(easymotion-j)
map ,w <Plug>(easymotion-w)
map ,b <Plug>(easymotion-b)
map ,j <Plug>(easymotion-j)
map ,k <Plug>(easymotion-k)
map ,h <Plug>(easymotion-linebackward)
map ,l <Plug>(easymotion-lineforward)
map ,. <Plug>(easymotion-repeat)
" map <Space>w <Plug>(easymotion-f)
" map <Space>b <Plug>(easymotion-F)
" map <Space> <Plug>(easymotion-bd-f)

" For Ctrlsf
nmap     <C-X>f <Plug>CtrlSFPrompt<C-R><C-W>
vmap     <C-X>f <Plug>CtrlSFVwordPath
vmap     <C-X>F <Plug>CtrlSFVwordExec
nmap     <C-X>n <Plug>CtrlSFCwordPath
nmap     <C-X>p <Plug>CtrlSFPwordPath
nnoremap <C-X>o :CtrlSFOpen<CR>
nnoremap <C-X>t :CtrlSFToggle<CR>
inoremap <C-X>t <Esc>:CtrlSFToggle<CR>
nnoremap <leader>sp :CtrlSF -filetype python<Space><C-R><C-W>
nnoremap <leader>sc :CtrlSF -filetype cc<Space><C-R><C-W>
nnoremap <leader>sz :CtrlSF -filetype cpp<Space><C-R><C-W>
nnoremap <leader>so :CtrlSF -filetype ocaml<Space><C-R><C-W>
nnoremap <leader>sm :CtrlSF -filetype markdown<Space><C-R><C-W>
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

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" SPECIFIC PLUGINS & SETTINGS
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" -------
" C & C++
" -------

" autocmd FileType c,cpp setlocal ts=4 sts=4 et
" autocmd FileType ocaml setlocal ts=2 sts=2 et
autocmd FileType c,cpp nmap <leader>gc :cs find c <cword><CR>
autocmd FileType c,cpp nmap <leader>gd :cs find g <cword><CR>

" ------
" Python
" ------

autocmd FileType python setlocal ts=4 sts=4 et

" Add the virtualenv's site-packages to vim path
if has('python')
py << EOF
import os.path
import sys
import vim
if 'VIRTUAL_ENV' in os.environ:
    project_base_dir = os.environ['VIRTUAL_ENV']
    sys.path.insert(0, project_base_dir)
    activate_this = os.path.join(project_base_dir, 'bin/activate_this.py')
    execfile(activate_this, dict(__file__=activate_this))
EOF
endif

" For jedi
let g:jedi#completions_enabled = 1
autocmd FileType python setlocal completeopt-=preview
" let g:jedi#auto_initialization = 1
" let g:jedi#auto_vim_configuration = 0
" let g:jedi#use_tabs_not_buffers = 0
" let g:jedi#use_splits_not_buffers = "left"
let g:jedi#popup_on_dot = 0
let g:jedi#popup_select_first = 1
let g:jedi#show_call_signatures = "2" 	" Set to 2 in command line
let g:jedi#goto_command = "<leader>gd"
let g:jedi#goto_assignments_command = "<leader>pa"
let g:jedi#documentation_command = "K"
let g:jedi#usages_command = "<leader>gc"
let g:jedi#completions_command = "<C-Space>"
let g:jedi#rename_command = "<leader>pr"

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
autocmd FileType markdown nmap <leader>ol :Toch<CR>

" For auto-pairs
let g:AutoPairsShortcutToggle = '<leader>ta'
