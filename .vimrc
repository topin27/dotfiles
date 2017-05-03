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
Plugin 'a.vim'
Plugin 'davidhalter/jedi-vim'
Plugin 'ervandew/supertab'
Plugin 'dyng/ctrlsf.vim'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'
" Plugin 'bufexplorer.zip'

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
set cc=81
set ls=2
autocmd FileType python setlocal ts=4 sts=4 et
autocmd FileType ocaml setlocal ts=2 sts=2 et
autocmd FileType c,cpp setlocal ts=2 sts=2 et
autocmd BufNewFile,BufReadPost *.md set filetype=markdown
autocmd FileType c,cpp nmap <leader>cf :cs find c <cword><CR>
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
map <F2> :NERDTreeToggle<CR>

" For tagbar
let g:tagbar_right=1
map <F3> :TagbarToggle<CR>
let g:tagbar_type_go = {
	\ 'ctagstype' : 'go',
	\ 'kinds'     : [
		\ 'p:package',
		\ 'i:imports:1',
		\ 'c:constants',
		\ 'v:variables',
		\ 't:types',
		\ 'n:interfaces',
		\ 'w:fields',
		\ 'e:embedded',
		\ 'm:methods',
		\ 'r:constructor',
		\ 'f:functions'
	\ ],
	\ 'sro' : '.',
	\ 'kind2scope' : {
		\ 't' : 'ctype',
		\ 'n' : 'ntype'
	\ },
	\ 'scope2kind' : {
		\ 'ctype' : 't',
		\ 'ntype' : 'n'
	\ },
	\ 'ctagsbin'  : 'gotags',
	\ 'ctagsargs' : '-sort -silent'
\ }

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
let g:syntastic_python_pylint_args='--disable=C0111,R0903,C0301'
" let g:syntastic_go_checkers = ['golint', 'govet']
let g:syntastic_mode_map = {
    \ 'mode': 'passive', 
    \ 'active_filetypes': [], 
    \ 'passive_filetypes': ['go']
\ }
map <F4> :SyntasticCheck<CR>

" For EasyMotion
" map <Leader><Leader>j <Plug>(easymotion-j)
map <Leader><leader>h <Plug>(easymotion-linebackward)
map <Leader><leader>l <Plug>(easymotion-lineforward)
map <Leader><leader>. <Plug>(easymotion-repeat)

" " For vim-go
" autocmd FileType go nmap <leader>gb <Plug>(go-build)
" autocmd FileType go nmap <leader>gr <Plug>(go-run)
" autocmd FileType go nmap <leader>gt <Plug>(go-test)
" autocmd FileType go nmap <leader>gc <Plug>(go-coverage-toggle)
" autocmd FileType go nmap <leader>cf <Plug>(go-referrers)
" autocmd FileType go nmap <leader>gl <Plug>(go-lint)
" autocmd FileType go nmap <leader>gv <Plug>(go-vet)
" autocmd FileType go nmap <leader>gn <Plug>(go-rename)
" autocmd FileType go nmap <leader>gi <Plug>(go-implements)
" autocmd FileType go nmap <leader>ga <Plug>(go-alternate-edit)
" autocmd FileType go nmap <leader>gf <Plug>(go-files)
" let g:go_get_update = 0
" let g:go_test_timeout = '10s'
" let g:go_fmt_autosave = 1
" let g:go_textobj_include_function_doc = 1
" let g:go_highlight_types = 1
" let g:go_highlight_functions = 1
" let g:go_highlight_methods = 1
" let g:go_list_type = "quickfix"
" " let g:go_def_mode = 'godef'	" using guru now
" let g:go_info_mode = 'guru'

" For jedi.vim
let g:jedi#completions_enabled = 1
autocmd FileType python setlocal completeopt-=preview

let g:jedi#auto_initialization = 1
let g:jedi#auto_vim_configuration = 0
let g:jedi#use_tabs_not_buffers = 0
" let g:jedi#use_splits_not_buffers = "left"
let g:jedi#popup_on_dot = 1
let g:jedi#popup_select_first = 1
let g:jedi#show_call_signatures = "1" 	" Set to 2 in command line

let g:jedi#goto_command = "<leader>gd"
let g:jedi#goto_assignments_command = "<leader>pa"
let g:jedi#documentation_command = "K"
let g:jedi#usages_command = "<leader>pn"
let g:jedi#completions_command = "<C-Space>"
let g:jedi#rename_command = "<leader>pr"

" For Ctrlsf
nmap     <C-X>f <Plug>CtrlSFPrompt
vmap     <C-X>f <Plug>CtrlSFVwordPath
vmap     <C-X>F <Plug>CtrlSFVwordExec
nmap     <C-X>n <Plug>CtrlSFCwordPath
nmap     <C-X>p <Plug>CtrlSFPwordPath
nnoremap <C-X>o :CtrlSFOpen<CR>
nnoremap <C-X>t :CtrlSFToggle<CR>
inoremap <C-X>t <Esc>:CtrlSFToggle<CR>
nnoremap <leader>agp :CtrlSF -filetype python <cword><CR>
nnoremap <leader>agc :CtrlSF -filetype cc <cword><CR>
nnoremap <leader>ago :CtrlSF -filetype ocaml <cword><CR>
let g:ctrlsf_ignore_dir = ['.git', '.svn', 'tags', 'cscope*.out']
let g:ctrlsf_default_view_mode = 'compact'

" For multiple-cursors
let g:multi_cursor_use_default_mapping=0
let g:multi_cursor_start_key='<F6>'
let g:multi_cursor_next_key='<C-n>'
let g:multi_cursor_prev_key='<C-p>'
let g:multi_cursor_skip_key='<C-x>'
let g:multi_cursor_quit_key='<Esc>'

" For ultisnips
let g:UltiSnipsExpandTrigger="yy"
let g:UltiSnipsListSnippets="yY"
let g:UltiSnipsJumpForwardTrigger="YY"
let g:UltiSnipsJumpBackwardTrigger="OO"
