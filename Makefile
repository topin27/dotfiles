SHELL := /bin/bash
UNAME := $(shell uname -s)
LN := ln -sfi

.PHONY: emacs vim sublime ag ctags tmux bash rime rg node

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

install: vim tmux bash

ag:
	$(LN) `pwd`/.agignore ~/.agignore

rg:
	if [ ! -f ~/bins/default/bin/rg ]; then \
		curl -OL https://github.com/BurntSushi/ripgrep/releases/download/14.1.0/ripgrep-14.1.0-x86_64-unknown-linux-musl.tar.gz && \
		tar xvf ripgrep-14.1.0-x86_64-unknown-linux-musl.tar.gz && \
		mv ripgrep-14.1.0-x86_64-unknown-linux-musl/rg ~/bins/default/bin/rg && \
		rm -rf ripgrep-14.1.0-x86_64-unknown-linux-musl*; \
	fi

CTAGS_DIR := $(shell realpath ~/bins/ctags/)
ctags-build:
	if [ ! -f $(CTAGS_DIR)/bin/ctags ]; then \
		mkdir -p build $(CTAGS_DIR) && \
		git clone https://github.com/universal-ctags/ctags --depth 1 build/ctags && \
		pushd build/ctags && \
		./autogen.sh && \
		./configure --prefix=$(CTAGS_DIR) && \
		make -j8 && \
		make install && \
		$(LN) ~/bins/ctags/bin/ctags ~/bins/default/bin/ctags && \
		popd && \
		rm -rf build; \
	fi

ctags: ctags-build
	test -L ~/.ctags.d/custom.ctags || \
		(mkdir -p ~/.ctags.d/ && $(LN) `pwd`/ctags ~/.ctags.d/custom.ctags)

tmux:
	test -L ~/.tmux.conf || \
		$(LN) `pwd`/tmux.conf ~/.tmux.conf

bash:
	mkdir -p ~/.config/bash/
	test -L ~/.config/bash/bashrc || \
		$(LN) `pwd`/bash/bashrc ~/.config/bash/bashrc

VIM_DIR := ~/bins/vim9/
vim-build:
	test -f $(VIM_DIR)/bin/vim || \
		(git clone --depth 1 https://github.com/vim/vim && \
		pushd vim && \
		./configure --with-features=huge --prefix=$(VIM_DIR) && \
		make -j8 && \
		make install && \
		$(LN) $(VIM_DIR)/bin/vim ~/bins/default/bin/vim && \
		$(LN) $(VIM_DIR)/bin/xxd ~/bins/default/bin/xxd && \
		popd)

NODE_VERSION := v20.10.0
NODE := node-$(NODE_VERSION)-linux-x64
NODE_DIR := ~/bins/node/
node:
	test -d $(NODE_DIR) || \
		(mkdir -p $(NODE_DIR) && \
		curl -OL https://nodejs.org/dist/$(NODE_VERSION)/$(NODE).tar.xz && \
		unxz $(NODE).tar.xz && \
		tar xvf $(NODE).tar && \
		mv $(NODE) $(NODE_DIR) && \
		rm -rf $(NODE).tar)

VIM := ~/.vim/
vim: vim-build ctags rg node  ## install vim config
	mkdir -p $(VIM)
	test -L $(VIM)/vimrc || 		$(LN) `pwd`/vim/vimrc $(VIM)/vimrc
	test -L $(VIM)/vimrc.featured || 	$(LN) `pwd`/vim/vimrc.featured $(VIM)/vimrc.featured
	test -L $(VIM)/vimrc.minimal || 	$(LN) `pwd`/vim/vimrc.minimal $(VIM)/vimrc.minimal
	test -L $(VIM)/coc-settings.json || 	$(LN) `pwd`/vim/coc-settings.json $(VIM)/coc-settings.json
	test -f $(VIM)/autoload/plug.vim || \
		curl -fLo $(VIM)/autoload/plug.vim --create-dirs \
		https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	test -L $(VIM)/code_snippets || (make -C code_snippets install)

rime:
	test -L ~/.config/ibus/rime || $(LN) `pwd`/squirrel/ ~/.config/ibus/rime

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
