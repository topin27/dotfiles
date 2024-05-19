SHELL := /bin/bash
LN := ln -sfi
CUR_DIR := $(shell pwd)

VIM_DIR := $(HOME)/bins/vim9/
VIM_CONF := $(HOME)/.vim/
CTAGS_DIR := $(HOME)/bins/ctags/
TMUX_DIR := $(HOME)/bins/tmux/
NODE_DIR := $(HOME)/bins/node/
BASH_CONF := $(HOME)/.config/bash/

RG_VERSION := 14.1.0
ifeq ($(UNAME),Darwin)
RG_NAME := ripgrep-$(RG_VERSION)-x86_64-apple-darwin
else
RG_NAME := ripgrep-$(RG_VERSION)-x86_64-unknown-linux-musl
endif
RG_URL := https://github.com/BurntSushi/ripgrep/releases/download/$(RG_VERSION)/$(RG_NAME).tar.gz


help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: install
install: pre-build \
	vim \
	tmux \
	bash \
	node \
	ctags \
	rg

.PHONY: uninstall
uninstall: \
	vim-uninstall \
	tmux-uninstall \
	bash-uninstall \
	node-uninstall \
	ctags-uninstall \
	rg-uninstall

.PHONY: pre-build
pre-build:
	sudo apt-get install -y autoconf pkg-config libncurses-dev curl libevent-dev yacc

.PHONY: vim
vim:  ## build vim and set the configure file
	# download
	if [ ! -d build/vim/ ]; then \
		mkdir -p build/vim/; \
		git clone --depth 1 https://github.com/vim/vim build/vim/; \
	fi
	# build
	if [ ! -f build/vim/src/vim ]; then \
		pushd build/vim/ && \
		./configure --with-features=huge --prefix=$(VIM_DIR) && \
		make -j4 && \
		popd; \
	fi
	# set configure file
	mkdir -p $(VIM_CONF)
	test -L $(VIM_CONF)/vimrc || \
		$(LN) $(CUR_DIR)/vim/vimrc $(VIM_CONF)/vimrc
	test -L $(VIM_CONF)/vimrc.featured || \
		$(LN) $(CUR_DIR)/vim/vimrc.featured $(VIM_CONF)/vimrc.featured
	test -L $(VIM_CONF)/vimrc.minimal || \
		$(LN) $(CUR_DIR)/vim/vimrc.minimal $(VIM_CONF)/vimrc.minimal
	test -L $(VIM_CONF)/coc-settings.json || \
		$(LN) $(CUR_DIR)/vim/coc-settings.json $(VIM_CONF)/coc-settings.json
	test -f $(VIM_CONF)/autoload/plug.vim || \
		curl -fLo $(VIM_CONF)/autoload/plug.vim --create-dirs \
		https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	# set snippets
	if [ ! $(shell ls code_snippets) ]; then \
		git submodule update --init --recursive; \
	fi
	test -L $(VIM_CONF)/code_snippets || (make -C code_snippets install)
	# install
	if [ ! -f $(VIM_DIR)/bin/vim ]; then \
		mkdir -p $(VIM_DIR); \
		pushd build/vim/ && make install && popd; \
	fi
	$(LN) $(VIM_DIR)/bin/vim $(HOME)/bins/default/bin/vim
	$(LN) $(VIM_DIR)/bin/xxd $(HOME)/bins/default/bin/xxd

.PHONY: vim-uninstall
vim-uninstall:
	rm -rf $(VIM_DIR) $(VIM_CONF)
	rm -rf $(HOME)/bins/default/bin/{vim,xxd}

.PHONY: ctags
ctags:  ## build and install ctags
	# download
	if [ ! -d build/ctags ]; then \
		mkdir -p build/ctags; \
		git clone https://github.com/universal-ctags/ctags --depth 1 build/ctags; \
	fi
	# build
	if [ ! -f build/ctags/ctags ]; then \
		pushd build/ctags && \
		./autogen.sh && \
		./configure --prefix=$(CTAGS_DIR) && \
		make -j4 && \
		popd; \
	fi
	# install
	if [ ! -f $(CTAGS_DIR)/bin/ctags ]; then \
		mkdir -p $(CTAGS_DIR); \
		pushd build/ctags/ && make install && popd && \
		$(LN) $(CTAGS_DIR)/bin/ctags $(HOME)/bins/default/bin/ctags && \
		$(LN) $(CTAGS_DIR)/bin/readtags $(HOME)/bins/default/bin/readtags; \
	fi

.PHONY: ctags-uninstall
ctags-uninstall:
	rm -rf $(CTAGS_DIR)
	rm -rf $(HOME)/bins/default/bin/{ctags,readtags}

.PHONY: rg
rg:
	# download
	if [ ! -f build/rg/rg.tar ]; then \
		mkdir build/rg/ && \
		curl -OL $(RG_URL) && \
		mv $(RG_NAME).tar.gz build/rg/rg.tar.gz && \
		pushd build/rg/ && gunzip rg.tar.gz; popd; \
	fi
	# install
	if [ ! -f $(HOME)/bins/default/bin/rg ]; then \
		pushd build/rg/ && \
		tar xvf rg.tar && \
		mv $(RG_NAME)/rg $(HOME)/bins/default/bin/rg && \
		rm -rf $(RG_NAME); \
	fi

.PHONY: rg-uninstall
rg-uninstall:
	rm -rf $(HOME)/bins/default/bin/rg

.PHONY: tmux
tmux:  ## build and install tmux
	# download
	if [ ! -d build/tmux ]; then \
		mkdir -p build/tmux; \
		git clone https://github.com/tmux/tmux --depth 1 build/tmux; \
	fi
	# build
	if [ ! -f build/tmux/tmux ]; then \
		pushd build/tmux && \
		./autogen.sh && \
		./configure --prefix=$(TMUX_DIR) && \
		make -j4 && \
		popd; \
	fi
	# install the configure file
	test -L $(HOME)/.tmux.conf || $(LN) $(CUR_DIR)/tmux.conf $(HOME)/.tmux.conf
	# install the binary
	if [ ! -f $(TMUX_DIR)/bin/tmux ]; then \
		mkdir -p $(TMUX_DIR); \
		pushd build/tmux && make install && popd; \
	fi
	$(LN) $(TMUX_DIR)/bin/tmux $(HOME)/bins/default/bin/tmux

.PHONY: tmux-uninstall
tmux-uninstall:
	rm -rf $(TMUX_DIR)
	rm -rf $(HOME)/bins/default/bin/tmux
	rm -rf $(HOME)/.tmux.conf

.PHONY: node
node:  ## build and install nodejs
	# download
	if [ ! -d build/node/ ]; then \
		mkdir -p build/node; \
		git clone https://github.com/nodejs/node.git --depth 1 build/node/; \
	fi
	# build
	if [ ! -f build/node/out/Release/node ]; then \
		pushd build/node && \
		./configure --prefix=$(NODE_DIR) && \
		make -j4 && \
		popd; \
	fi
	# install
	if [ ! -f $(NODE_DIR)/bin/node ]; then \
		mkdir -p $(NODE_DIR); \
		pushd build/node && make install && popd; \
	fi

.PHONY: node-uninstall
node-uninstall:
	rm -rf $(NODE_DIR)
	rm -rf $(HOME)/.npm/

.PHONY: bash
bash:
	mkdir -p $(BASH_CONF)
	test -L $(BASH_CONF)/bashrc || \
		$(LN) $(CUR_DIR)/bash/bashrc $(BASH_CONF)/bashrc

.PHONY: bash-uninstall
bash-uninstall:
	rm -rf $(BASH_CONF)

.PHONY: rime
rime:
	mkdir -p $(HOME)/.config/ibus/
	test -L $(HOME)/.config/ibus/rime || \
		$(LN) $(CUR_DIR)/squirrel/ $(HOME)/.config/ibus/rime

.PHONY: emacs
emacs:  ## install emacs config
	mkdir -p $(HOME)/.emacs.d/
	$(LN) $(CUR_DIR)/emacs/init.el $(HOME)/.emacs.d/init.el

.PHONY: emacs-uninstall
emacs-uninstall:
	rm -rf $(HOME)/.emacs.d/

ifeq ($(UNAME),Darwin)
sublime:
	ln -s $(HOME)/Library/Application\ Support/Sublime\ Text\ 3/Packages/User/Markdown.sublime-settings \
		sublime/Markdown.sublime-settings
	ln -s $(HOME)/Library/Application\ Support/Sublime\ Text\ 3/Packages/User/Preferences.sublime-settings \
		sublime/Preferences.sublime-settings
	ln -s $(HOME)/Library/Application\ Support/Sublime\ Text\ 3/Packages/User/Markdown.sublime-build \
		sublime/markdown.sublime-build
else
sublime:
	echo "The sublime install is not support to current OS platform!!"
endif

clean:
	for i in $(shell ls build); do \
		make -C build/$$i clean distclean; \
	done
	make -C build/vim/ distclean
	make -C build/node/ distclean
