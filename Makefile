UNAME := $(shell uname -s)

.PHONY: tmux ag ctags vim emacs
all: tmux ag ctags vim

tmux:
	ln -is `pwd`/.tmux.conf ~/.tmux.conf

ag:
	ln -is `pwd`/.agignore ~/.agignore

ctags:
	mkdir -p ~/.ctags.d/
	ln -is `pwd`/.ctags ~/.ctags.d/custom.ctags

vim:
	curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
		https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	ln -is `pwd`/vim/.vimrc ~/.vim/vimrc
	ln -is `pwd`/vim/.vimrc.featured ~/.vim/vimrc.featured
	ln -is `pwd`/vim/.vimrc.minimal ~/.vim/vimrc.minimal

emacs:
	mkdir -p ~/.emacs.d
	ln -s `pwd`/emacs/.emacs ~/.emacs.d/init.el

ifeq ($(UNAME),Darwin)
sublime:
	ln -s ~/Library/Application\ Support/Sublime\ Text\ 3/Packages/User/Markdown.sublime-settings \
		sublime/Markdown.sublime-settings
	ln -s ~/Library/Application\ Support/Sublime\ Text\ 3/Packages/User/Preferences.sublime-settings \
		sublime/Preferences.sublime-settings
	ln -s ~/Library/Application\ Support/Sublime\ Text\ 3/Packages/User/Markdown.sublime-build \
		sublime/markdown.sublime-build
else
sublime:
	echo "The sublime install is not support to current OS platform!!"
endif
