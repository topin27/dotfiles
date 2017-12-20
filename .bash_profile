# for color
export CLICOLOR=1
export PS1='\[\033[01;33m\]\u@\h\[\033[01;31m\] \W\$\[\033[00m\]'
alias grep='grep --color=always'

if [ -d $HOME/Library/Python/2.7/bin ]; then
	export PATH=$HOME/Library/Python/2.7/bin:$PATH
fi

export WORKON_HOME='~/.virtualenvs'
source ~/Library/Python/2.7/bin/virtualenvwrapper.sh
