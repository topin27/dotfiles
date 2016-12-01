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

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required


" Basic settings
set nu
set ai
set hlsearch
set noexpandtab
set ts=8
set cc=81
autocmd FileType python setlocal ts=4 sts=4
noremap <tab> <c-w><c-w>

" For NERDTree
let NERDTreeHighlightCursorline=1
map <F2> :NERDTreeToggle<CR>

" For tagbar
let g:tagbar_right=1
map <F3> :TagbarToggle<CR>

" For ctrlp
let g:ctrlp_map = '<leader>p'
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
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0
let g:syntastic_check_on_w = 0
let g:syntastic_python_checkers=['pylint']
map <F5> :SyntasticCheck<CR>
