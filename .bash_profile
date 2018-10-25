# for color
export CLICOLOR=1
export PS1='\[\033[01;33m\]\u@\h\[\033[01;31m\] \W\$\[\033[00m\]'
alias grep='grep --color=always'
alias rm='rm -i'
# alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs-x86_64-10_9 -nw'
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

# if [ -d $HOME/Library/Python/2.7/bin ]; then
# 	export PATH=$HOME/Library/Python/2.7/bin:$PATH
# fi

my-workon () {
    case $1 in
	      "python")
		  export PATH=$HOME/Library/Python/2.7/bin:$PATH
	          ;;
	      "java")
	          ;;
	  "scala")
		  ;;
	      *)
	          echo "Usage: my-workon <java|python|scala>"
	          ;;
    esac
}
