set nocompatible              " be iMproved, required
filetype off                  " required

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
Plugin 'grep.vim'
Plugin 'vim-syntastic/syntastic'
Plugin 'fholgado/minibufexpl.vim'
Plugin 'easymotion/vim-easymotion'
Plugin 'tpope/vim-markdown'

" All of your Plugins must be added before the following line
call vundle#end()            " required
" filetype plugin indent on    " required


" Basic settings
syntax on
set nu
set ai
set hlsearch
set noexpandtab
set ts=8
set cc=81
autocmd FileType python setlocal ts=4 sts=4 expandtab
autocmd FileType ocaml setlocal ts=2 sts=2
" noremap <tab> <c-w><c-w>
cnoremap <C-A> <Home>
cnoremap <C-E> <End>
cnoremap <C-F> <Right>
cnoremap <C-B> <Left>
cnoremap <C-P> <Up>
cnoremap <C-N> <Down>
nmap <leader>bb :ls<CR>:buffer<Space>

" For NERDTree
let NERDTreeHighlightCursorline=1
map <F2> :NERDTreeToggle<CR>

" For tagbar
let g:tagbar_right=1
map <F3> :TagbarToggle<CR>

" For ctrlp
let g:ctrlp_map = '<leader>f'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_custom_ignore = {
    \ 'dir':  '\v[\/]\.(git|hg|svn|rvm)$',
    \ 'file': '\v\.(exe|so|dll|zip|tar|tar.gz|pyc)$',
    \ }

" For grep.vim
let Grep_Skip_Dirs = '.git .svn'
let Grep_Skip_Files = 'tags'
map <F4> :Rgrep<CR><CR><CR><CR>

" For synctastic
" set statusline+=%#warningmsg#
" set statusline+=%{SyntasticStatuslineFlag()}
" set statusline+=%*
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0
let g:syntastic_check_on_w = 0
let g:syntastic_python_checkers=['pylint']
let g:syntastic_mode_map = {
    \ 'mode': 'passive', 
    \ 'active_filetypes': [], 
    \ 'passive_filetypes': []
    \ }
map <F5> :SyntasticCheck<CR>

" For minibufexpl

" For EasyMotion
" map <Leader><Leader>j <Plug>(easymotion-j)
map <Leader><leader>h <Plug>(easymotion-linebackward)
map <Leader><leader>l <Plug>(easymotion-lineforward)
map <Leader><leader>. <Plug>(easymotion-repeat)

" For markdown
autocmd BufNewFile,BufReadPost *.md set filetype=markdown
let g:markdown_fenced_languages = ['html', 'python', 'bash=sh']
" let g:markdown_syntax_conceal = 0
