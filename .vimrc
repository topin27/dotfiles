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
Plugin 'mileszs/ack.vim'
Plugin 'davidhalter/jedi-vim'
Plugin 'ervandew/supertab'
Plugin 'dyng/ctrlsf.vim'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required


" Basic settings
syntax on
set nu
set ai
set hlsearch
set noexpandtab
set ts=8
set cc=81
set ls=2
autocmd FileType python setlocal ts=4 sts=4 expandtab
autocmd FileType ocaml setlocal ts=2 sts=2
" autocmd FileType c,cpp setlocal ts=2 sts=2 expandtab
autocmd FileType c,cpp nmap <leader>cf :cs find c <cword><CR>
autocmd FileType python nmap <leader>cf :cs find c <cword><CR>
" noremap <tab> <c-w><c-w>
cnoremap <C-A> <Home>
cnoremap <C-E> <End>
cnoremap <C-F> <Right>
cnoremap <C-B> <Left>
cnoremap <C-P> <Up>
cnoremap <C-N> <Down>
nmap <leader>bb :ls<CR>:buffer<Space>
nmap <leader>ln :lnext<CR>
nmap <leader>lp :lprevious<CR>
nmap <leader>cn :cnext<CR>
nmap <leader>cp :cprevious<CR>
map <F5>t :!ctags -R<CR><CR>
map <F5>s :!cscope -Rbk<CR><CR>:cs reset<CR>

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
let g:syntastic_go_checkers = ['golint', 'govet']
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

" For ack.vim
if executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif
cnoreabbrev Ack Ack!
nnoremap <Leader>agp :Ack! --python<Space>
nnoremap <Leader>agc :Ack! --cc<Space>
nnoremap <Leader>ago :Ack! --ocaml<Space>
nnoremap <Leader>aga :Ack!<Space>

" For jedi.vim
let g:jedi#completions_enabled = 1
autocmd FileType python setlocal completeopt-=preview

let g:jedi#auto_initialization = 1
let g:jedi#auto_vim_configuration = 0
let g:jedi#use_tabs_not_buffers = 1
" let g:jedi#use_splits_not_buffers = "left"
let g:jedi#popup_on_dot = 1
let g:jedi#popup_select_first = 1
let g:jedi#show_call_signatures = "1" 	" Set to 2 in command line

let g:jedi#goto_command = "<leader>pg"
let g:jedi#goto_assignments_command = "<leader>pa"
let g:jedi#goto_definitions_command = "<leader>pd"
let g:jedi#documentation_command = "K"
let g:jedi#usages_command = "<leader>pn"
let g:jedi#completions_command = "<C-Space>"
let g:jedi#rename_command = "<leader>pr"

" For Ctrlsf
nmap     <C-F>f <Plug>CtrlSFPrompt
vmap     <C-F>f <Plug>CtrlSFVwordPath
vmap     <C-F>F <Plug>CtrlSFVwordExec
nmap     <C-F>n <Plug>CtrlSFCwordPath
nmap     <C-F>p <Plug>CtrlSFPwordPath
nnoremap <C-F>o :CtrlSFOpen<CR>
nnoremap <C-F>t :CtrlSFToggle<CR>
inoremap <C-F>t <Esc>:CtrlSFToggle<CR>
