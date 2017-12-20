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
Plugin 'vim-syntastic/syntastic'
Plugin 'easymotion/vim-easymotion'
Plugin 'dyng/ctrlsf.vim'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'
Plugin 'Valloric/YouCompleteMe'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin on    " required


" Basic settings
syntax on
set nu
set ai
set hlsearch
set noet
set ts=8
" set cc=81
set ls=2
autocmd FileType python setlocal ts=4 sts=4 et
autocmd FileType c,cpp setlocal ts=4 sts=4 et
autocmd FileType ocaml setlocal ts=2 sts=2 et
autocmd BufNewFile,BufReadPost *.md set filetype=markdown
autocmd FileType c,cpp nmap <leader>gc :cs find c <cword><CR>
autocmd FileType c,cpp nmap <leader>gd :cs find g <cword><CR>
" noremap <tab> <c-w><c-w>
cnoremap <C-A> <Home>
cnoremap <C-E> <End>
cnoremap <C-F> <Right>
cnoremap <C-B> <Left>
cnoremap <C-P> <Up>
cnoremap <C-N> <Down>
" nmap <leader>bb :ls<CR>:buffer<Space>
nmap <leader>ln :lnext<CR>
nmap <leader>lp :lprevious<CR>
nmap <leader>cn :cnext<CR>
nmap <leader>cp :cprevious<CR>

" For NERDTree
let NERDTreeIgnore=['\.pyc$', '\~$'] "ignore files in NERDTree
let NERDTreeHighlightCursorline=1
map <F2> :NERDTreeToggle<CR><C-W>h

" For tagbar
let g:tagbar_right=1
map <F3> :TagbarToggle<CR><C-W>l

" For ctrlp
let g:ctrlp_map = '<leader>f'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_custom_ignore = {
    \ 'dir':  '\v[\/]\.(git|hg|svn|rvm)$',
    \ 'file': '\v\.(exe|so|dll|zip|tar|tar.gz|pyc)$',
\ }
nmap <leader>b :CtrlPBuffer<CR>

" For synctastic
" set statusline+=%#warningmsg#
" set statusline+=%{SyntasticStatuslineFlag()}
" set statusline+=%*
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0
let g:syntastic_check_on_w = 0
let g:syntastic_python_checkers = ['pylint']
let g:syntastic_c_checkers = ['gcc', 'clang']
let g:syntastic_ocaml_checkers = ['merlin']
let g:syntastic_python_pylint_args='--disable=C0111,R0903,C0301'
let g:syntastic_mode_map = {'mode': 'passive', 'active_filetypes': [],'passive_filetypes': []}
map <F4> :SyntasticToggleMode<CR>

" For EasyMotion
" map <Leader><Leader>j <Plug>(easymotion-j)
map <Space>w <Plug>(easymotion-w)
map <Space>b <Plug>(easymotion-b)
map <Space>j <Plug>(easymotion-j)
map <Space>k <Plug>(easymotion-k)
map <Leader><leader>. <Plug>(easymotion-repeat)

" For Ctrlsf
nmap     <C-X>f <Plug>CtrlSFPrompt
vmap     <C-X>f <Plug>CtrlSFVwordPath
vmap     <C-X>F <Plug>CtrlSFVwordExec
nmap     <C-X>n <Plug>CtrlSFCwordPath
nmap     <C-X>p <Plug>CtrlSFPwordPath
nnoremap <C-X>o :CtrlSFOpen<CR>
nnoremap <C-X>t :CtrlSFToggle<CR>
inoremap <C-X>t <Esc>:CtrlSFToggle<CR>
nnoremap <leader>agp :CtrlSF -filetype python<Space>
nnoremap <leader>agc :CtrlSF -filetype cc<Space>
nnoremap <leader>ago :CtrlSF -filetype ocaml<Space>
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

" For ocaml
let g:opamshare = substitute(system('opam config var share'),'\n$','','''')
execute "set rtp+=" . g:opamshare . "/merlin/vim"
autocmd FileType ocaml nmap <leader>gd :MerlinLocate<CR>
autocmd FileType ocaml map <F3> :MerlinOutline<CR>

" For ycm
let g:ycm_auto_trigger = 1
set completeopt-=preview " Disable preview window when complete
let g:ycm_error_symbol = '>>'
let g:ycm_warning_symbol = '>*'
nnoremap <leader>gd :YcmCompleter GoTo<CR>
nnoremap K :YcmCompleter GetDoc<CR><C-W>k
nnoremap <leader>ty :let g:ycm_auto_trigger=0<CR>
nnoremap <leader>tY :let g:ycm_auto_trigger=1<CR>
let g:ycm_python_binary_path = 'python'
autocmd FileType c,cpp nnoremap <F4> :YcmForceCompileAndDiagnostics<CR>
" autocmd FileType c,cpp nnoremap <F4> :YcmDiags<CR>
let g:ycm_min_num_of_chars_for_completion = 3
let g:ycm_filetype_whitelist = { 'python': 1, 'c': 1, 'cpp': 1 }
let g:ycm_show_diagnostics_ui = 1
let g:ycm_autoclose_preview_window_after_completion = 0
let g:ycm_autoclose_preview_window_after_insertion = 1
let g:ycm_key_list_stop_completion = ['<C-y>']
let g:ycm_key_detailed_diagnostics = '<leader>ed'
let g:ycm_global_ycm_extra_conf = '~/.vim/bundle/YouCompleteMe/third_party/ycmd/cpp/ycm/.ycm_extra_conf.py'
let g:ycm_use_ultisnips_completer = 0
