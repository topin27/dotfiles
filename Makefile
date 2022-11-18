UNAME := $(shell uname -s)
LN := ln -sfi

.PHONY: emacs vim sublime

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

update:  ## refresh the link to the installed config
	$(LN) `pwd`/.agignore ~/.agignore
	mkdir -p ~/.ctags.d/ && $(LN) `pwd`/ctags ~/.ctags.d/custom.ctags
	$(LN) `pwd`/tmux.conf ~/.tmux.conf

vim:  ## install vim config
	mkdir -p ~/.config/nvim/
	$(LN) `pwd`/vim/coc-settings.json ~/.config/nvim/coc-settings.json
	$(LN) `pwd`/vim/init.vim ~/.config/nvim/init.vim
	mkdir -p ~/.vim/
	$(LN) `pwd`/vim/vimrc ~/.vim/vimrc
	$(LN) `pwd`/vim/vimrc.featured ~/.vim/vimrc.featured
	$(LN) `pwd`/vim/vimrc.minimal ~/.vim/vimrc.minimal

emacs:  ## install emacs config
	mkdir -p ~/.emacs.d/
	$(LN) `pwd`/emacs/init.el ~/.emacs.d/init.el

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

clean:
	rm -rf tmp_for_vim
