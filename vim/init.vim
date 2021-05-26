set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath=&runtimepath
let g:python3_host_prog = $HOME . "/.venvs/nvim/bin/python"
source ~/.vim/vimrc.featured
