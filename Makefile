UNAME := $(shell uname -s)

.PHONY: tmux ag ctags vim emacs
all: tmux ag ctags vim

tmux:
	ln -is `pwd`/.tmux.conf ~/.tmux.conf

ag:
	ln -is `pwd`/.agignore ~/.agignore

ctags:
	mkdir -p ~/.ctags.d/
	cp .ctags ~/.ctags.d/custom.ctags

vim:
	curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
		https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	git clone --depth 1 https://github.com/ervandew/supertab.git ~/.vim/plugged/supertab/
	git clone --depth 1 https://github.com/scrooloose/nerdtree.git ~/.vim/plugged/nerdtree/
	git clone --depth 1 https://github.com/majutsushi/tagbar.git ~/.vim/plugged/tabular/
	git clone --depth 1 https://github.com/SirVer/ultisnips.git ~/.vim/plugged/ultisnips/
	git clone --depth 1 https://github.com/mileszs/ack.vim ~/.vim/plugged/ack.vim
	git clone --depth 1 https://github.com/tpope/vim-surround ~/.vim/plugged/vim-surround/
	git clone --depth 1 https://github.com/godlygeek/tabular ~/.vim/plugged/tabular/
	git clone --depth 1 https://github.com/amix/open_file_under_cursor.vim ~/.vim/plugged/open_file_under_cursor/
	git clone --depth 1 https://github.com/topin27/taskpaper.vim ~/.vim/plugged/supertab/taskpaper.vim
	git clone --depth 1 https://github.com/topin27/JavaImp.vim ~/.vim/plugged/JavaImp.vim
	git clone --depth 1 https://github.com/junegunn/fzf.vim ~/.vim/plugged/fzf.vim
	git clone --depth 1 https://github.com/pangloss/vim-javascript ~/.vim/plugged/vim-javascript
	git clone --depth 1 https://github.com/tpope/vim-fugitive ~/.vim/plugged/vim-fugitive
	git clone --depth 1 https://github.com/vim-pandoc/vim-pandoc-syntax ~/.vim/plugged/vim-pandoc-syntax
	git clone --depth 1 https://github.com/aklt/plantuml-syntax ~/.vim/plugged/plantuml-syntax
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
