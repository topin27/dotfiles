SHELL := /bin/bash
LN := ln -sfi

.PHONY: emacs vim sublime ag ctags tmux bash rime rg node

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: install
install: pre-build vim tmux rime bash

.PHONY: pre-build
pre-build:
	mkdir -p ~/bins/default/bin/
	mkdir -p ~/bins/{ctags,vim,tmux,node}
	mkdir -p build
	sudo apt-get install -y autoconf pkg-config libncurses-dev curl libevent-dev yacc

VIM_DIR := $(shell realpath ~/bins/vim9/)
vim-build:
	if [ ! -d build/vim/ ]; then \
		git clone --depth 1 https://github.com/vim/vim build/vim/; \
	fi
	if [ ! -f build/vim/src/vim ]; then \
		pushd build/vim/ && \
		./configure --with-features=huge --prefix=$(VIM_DIR) && \
		make -j8 && \
		popd; \
	fi

VIM_CONF := ~/.vim/
vim-config:
	mkdir -p $(VIM_CONF)
	test -L $(VIM_CONF)/vimrc || $(LN) `pwd`/vim/vimrc $(VIM_CONF)/vimrc
	test -L $(VIM_CONF)/vimrc.featured || $(LN) `pwd`/vim/vimrc.featured $(VIM_CONF)/vimrc.featured
	test -L $(VIM_CONF)/vimrc.minimal || $(LN) `pwd`/vim/vimrc.minimal $(VIM_CONF)/vimrc.minimal
	test -L $(VIM_CONF)/coc-settings.json || $(LN) `pwd`/vim/coc-settings.json $(VIM_CONF)/coc-settings.json
	test -f $(VIM_CONF)/autoload/plug.vim || \
		curl -fLo $(VIM_CONF)/autoload/plug.vim --create-dirs \
		https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	git submodule update --init --recursive
	test -L $(VIM_CONF)/code_snippets || (make -C code_snippets install)

vim: vim-build vim-config node ctags rg
	if [ ! -f ~/bins/vim9/bin/vim ]; then \
		pushd build/vim/ && make install && popd; \
	fi
	$(LN) $(VIM_DIR)/bin/vim ~/bins/default/bin/vim
	$(LN) $(VIM_DIR)/bin/xxd ~/bins/default/bin/xxd

CTAGS_DIR := $(shell realpath ~/bins/ctags/)
ctags-build:
	if [ ! -d build/ctags ]; then \
		git clone https://github.com/universal-ctags/ctags --depth 1 build/ctags; \
	fi
	if [ ! -f build/ctags/ctags ]; then \
		pushd build/ctags && \
		./autogen.sh && \
		./configure --prefix=$(CTAGS_DIR) && \
		make -j8 && \
		popd; \
	fi

ctags-config:
	test -L ~/.ctags.d/custom.ctags || \
		(mkdir -p ~/.ctags.d/ && $(LN) `pwd`/ctags ~/.ctags.d/custom.ctags)

ctags: ctags-build ctags-config
	if [ ! -f ~/bins/ctags/bin/ctags ]; then \
		pushd build/ctags/ && make install && popd; \
	fi
	$(LN) ~/bins/ctags/bin/ctags ~/bins/default/bin/ctags

rg:
	if [ ! -f build/rg/rg.tar ]; then \
		mkdir build/rg/ && \
		curl -OL https://github.com/BurntSushi/ripgrep/releases/download/14.1.0/ripgrep-14.1.0-x86_64-unknown-linux-musl.tar.gz && \
		mv ripgrep-14.1.0-x86_64-unknown-linux-musl.tar.gz build/rg/rg.tar.gz && \
		pushd build/rg/ && gunzip rg.tar.gz; popd; \
	fi
	if [ ! -f ~/bins/default/bin/rg ]; then \
		pushd build/rg/ && \
		tar xvf rg.tar && \
		mv ripgrep-14.1.0-x86_64-unknown-linux-musl/rg ~/bins/default/bin/rg && \
		rm -rf ripgrep-14.1.0-x86_64-unknown-linux-musl/; \
	fi

TMUX_DIR := $(shell realpath ~/bins/tmux/)
tmux-build:
	if [ ! -d build/tmux ]; then \
		git clone https://github.com/tmux/tmux --depth 1 build/tmux; \
	fi
	if [ ! -f build/tmux/tmux ]; then \
		pushd build/tmux && \
		./autogen.sh && \
		./configure --prefix=$(TMUX_DIR) && \
		make -j8 && \
		popd; \
	fi

tmux-config:
	test -L ~/.tmux.conf || $(LN) `pwd`/tmux.conf ~/.tmux.conf

tmux: tmux-build tmux-config
	if [ ! -f ~/bins/tmux/bin/tmux ]; then \
		pushd build/tmux && make install && popd; \
	fi
	$(LN) ~/bins/tmux/bin/tmux ~/bins/default/bin/tmux

NODE_DIR := $(shell realpath ~/bins/node/)
node:
	if [ ! -f build/node/node.tar ]; then \
		mkdir -p build/node/ && \
		curl -OL https://nodejs.org/download/release/v18.19.0/node-v18.19.0-linux-x64.tar.xz && \
		mv node-v18.19.0-linux-x64.tar.xz build/node/node.tar.xz && \
		pushd build/node/ && unxz node.tar.xz && popd; \
	fi
	if [ ! -f $(NODE_DIR)/bin/node ]; then \
		pushd build/node/ && \
		tar xvf node.tar && \
		mv node-v18.19.0-linux-x64/* $(NODE_DIR)/. && \
		rm -rf node-v18.19.0-linux-x64 && \
		popd; \
	fi

bash:
	mkdir -p ~/.config/bash/
	test -L ~/.config/bash/bashrc || \
		$(LN) `pwd`/bash/bashrc ~/.config/bash/bashrc

rime:
	mkdir -p ~/.config/ibus/
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
