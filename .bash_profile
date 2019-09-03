# for color
export CLICOLOR=1
export PS1='\[\033[01;33m\]\u@\h\[\033[01;31m\] \W\$\[\033[00m\]'
alias grep='grep --color=always'
alias rm='rm -i'
alias untar='tar xvf'
alias ping='ping -c 5'
alias www='python -m SimpleHTTPServer 8000'
alias ipe='curl ipinfo.io/ip'
alias ipi='ipconfig getifaddr en0'
alias gits='git status'
alias gitc='git commit -m'
alias gitp='git push'
alias gitf='git pull'
alias gitd='git diff'
alias gita='git add'
alias vi='vim -u <(echo source /usr/share/vim/vimrc; cat ~/.vimrc.minimal)'
alias nvi='vim -u <(echo source /usr/share/vim/vimrc; cat ~/.vimrc.featured)'

my-md2html() {
	pandoc -f markdown -t html --mathml --toc -N --self-contained -s $1 -o $2
}
