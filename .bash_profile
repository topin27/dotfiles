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

my-workon () {
	case $1 in
		"python")
			# export PATH=$HOME/Library/Python/2.7/bin:$PATH
			export PATH=$HOME/.local/bin/:$PATH
			;;
		"java")
			export PATH=$HOME/bins/maven/bin/:$PATH
			;;
		"scala")
			export PATH=$HOME/bins/sbt/bin/:$HOME/bins/scala/bin/:$PATH
			;;
		*)
			echo "Usage: my-workon <java|python|scala>"
			;;
	esac
}

my-workoff () {
	case $1 in 
		"python")
			PATH=$(echo $PATH | sed -e "s;:$HOME/.local/bin/;;" -e "s;$HOME/.local/bin/:;;")
			;;
		"java")
			PATH=$(echo $PATH | sed -e "s;:$HOME/bins/maven/bin/;;" -e "s;$HOME/bins/maven/bin/:;;")
			;;
		"scala")
			PATH=$(echo $PATH | sed -e "s;:$HOME/bins/sbt/bin/;;" -e "s;$HOME/bins/sbt/bin/:;;")
			PATH=$(echo $PATH | sed -e "s;:$HOME/bins/scala/bin/;;" -e "s;$HOME/bins/scala/bin/:;;")
			;;
		*)
			echo "Usage: my-workoff <java|python|scala>"
			;;
	esac
}
