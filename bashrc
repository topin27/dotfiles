# for color
export CLICOLOR=1
export PS1='\[\033[01;33m\]\u@\h\[\033[01;31m\] \W\$\[\033[00m\]'
alias grep='grep --color=always'
alias rm='rm -i'
alias ipe='curl ipinfo.io/ip'
alias vi='vim -u <(echo source /usr/share/vim/vimrc; cat ~/.vim/vimrc.minimal)'
alias nvim='PATH=~/bins/node/bin:$PATH vim -u <(echo source /usr/share/vim/vimrc; cat ~/.vim/vimrc.featured)'
alias ll='ls -lh'
alias la='ls -ah'
alias lla='ls -lah'

export FZF_DEFAULT_COMMAND='ag --hidden -g ""'
export FZF_DEFAULT_OPTS='--bind ctrl-p:preview-up,ctrl-n:preview-down,alt-k:page-up,alt-j:page-down'

my-md2html() {
	if [ $# -eq 1 ]; then
		pandoc -f markdown -t html -s $1 -o `basename $1 .md`.html -N --toc --mathml
	elif [ $# -eq 2 ]; then
		pandoc -f markdown -t html -s $1 -o $2 -N --toc --mathml
	else
		echo "Error: Wrong usage!"
		echo "Usage: my-md2html <md-file> [html-file]"
		return 1
	fi
	return 0
}

my-addpath() {
	if [ $# -eq 0 ]; then
		export PATH=`pwd`:$PATH
		echo "Added \"`pwd`\" to \$PATH"
	else
		# do validation on arguments
		for i in $@; do
			# In linux, we can just use `realpath` command to get the absolute path
			# of a file, but this command is not exist on Mac OS X, so use the python
			# version instead.
			local candidate_path=`python -c "import os; print(os.path.realpath('$i'))"`
			if [ ! -d $candidate_path ]; then
				echo "Error: \"$i\" seems not a valid path"
				return 1
			fi
		done

		for i in $@; do
			local candidate_path=`python -c "import os; print(os.path.realpath('$i'))"`
			export PATH=$candidate_path:$PATH
			echo "Added \"$candidate_path\" to \$PATH"
		done
	fi
	return 0
}

export PATH=~/bins/default/bin/:$PATH
