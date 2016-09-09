set nu
set ai
set noexpandtab
set ts=8
syntax on
set cc=81
set hlsearch

autocmd FileType python setlocal ts=4 sts=4 expandtab
autocmd FileType lua setlocal ts=4 sts=4 expandtab

nmap <F8> :TagbarToggle<CR>
nmap <F3> :NERDTreeToggle<CR>
nmap <F4> :Rgrep<CR><CR><CR><CR>
