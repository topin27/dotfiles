SHELL := /bin/bash
LN := ln -sfi
CUR_DIR := $(shell pwd)

VIM_DIR := $(HOME)/bins/vim9/
VIM_CONF := $(HOME)/.vim
CTAGS_DIR := $(HOME)/bins/ctags/
TMUX_DIR := $(HOME)/bins/tmux/
NODE_DIR := $(HOME)/bins/node/
BASH_CONF := $(HOME)/.config/bash/

RG_VER ?= 14.1.0
NODE_VER ?= v16.20.2
ifeq ($(shell uname -s),Darwin)
RG_ARCH ?= x86_64-apple-darwin
NODE_ARCH ?= darwin-x64
INSTALLER := brew install
DEP_PKGS := autoconf pkg-config ncurses libevent python3
else
RG_ARCH ?= x86_64-unknown-linux-musl
NODE_ARCH ?= linux-x64
INSTALLER := apt-get install -y
DEP_PKGS := autoconf pkg-config libncurses-dev curl libevent-dev
endif
RG_NAME := ripgrep-$(RG_VER)-$(RG_ARCH)
RG_URL := https://github.com/BurntSushi/ripgrep/releases/download/$(RG_VER)/$(RG_NAME).tar.gz
NODE_NAME := node-$(NODE_VER)-$(NODE_ARCH)
NODE_URL := https://nodejs.org/dist/$(NODE_VER)/$(NODE_NAME).tar.gz

.PHONY: deps download build config install uninstall clean

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

deps:  ## install the build dependencies
	$(INSTALLER) $(DEP_PKGS)

download:  ## download the source and the binaries of modules
	if [ ! -d build/vim/ ]; then \
		mkdir -p build/vim/; \
		git clone --depth 1 https://github.com/vim/vim build/vim/; \
	fi
	if [ ! -d build/ctags ]; then \
		mkdir -p build/ctags; \
		git clone https://github.com/universal-ctags/ctags --depth 1 build/ctags; \
	fi
	if [ ! -d build/tmux ]; then \
		mkdir -p build/tmux; \
		git clone https://github.com/tmux/tmux --depth 1 build/tmux; \
	fi
	if [ ! -f build/node_prebuilt/node.tar.gz ]; then \
		mkdir -p build/node_prebuilt; \
		curl -kL $(NODE_URL) -o build/node_prebuilt/node.tar.gz; \
	fi
	if [ ! -f build/rg/rg.tar.gz ]; then \
		mkdir -p build/rg/ && \
		curl -kL $(RG_URL) -o build/rg/rg.tar.gz; \
	fi

build:  ## build source or unpackage the binaries of modules
	if [ ! -f build/vim/src/vim ]; then \
		pushd build/vim/ && \
		(./configure --with-features=huge --disable-gui \
			--enable-python3interp \
			--enable-multibyte --prefix=$(VIM_DIR) && \
		 make -j4); \
		popd; \
	fi
	if [ ! -f build/ctags/ctags ]; then \
		pushd build/ctags && \
		(./autogen.sh && ./configure --prefix=$(CTAGS_DIR) && make -j4); \
		popd; \
	fi
	if [ ! -f build/tmux/tmux ]; then \
		pushd build/tmux && \
		(./autogen.sh && ./configure --prefix=$(TMUX_DIR) --disable-utf8proc && make -j4); \
		popd; \
	fi
	if [ ! -f build/node_prebuilt/$(NODE_NAME)/bin/node ]; then \
		pushd build/node_prebuilt/ && \
		tar xvf node.tar.gz; \
		popd; \
	fi
	if [ ! -f build/rg/$(RG_NAME)/rg ]; then \
		pushd build/rg/ && \
		tar xvf rg.tar.gz; \
		popd; \
	fi

install:  ## install the built binaries of modules
	if [ ! -f $(VIM_DIR)/bin/vim ]; then \
		mkdir -p $(VIM_DIR); \
		pushd build/vim/ && make install; popd; \
		$(LN) $(VIM_DIR)/bin/vim $(HOME)/bins/default/bin/vim; \
		$(LN) $(VIM_DIR)/bin/xxd $(HOME)/bins/default/bin/xxd; \
	fi
	if [ ! -f $(CTAGS_DIR)/bin/ctags ]; then \
		mkdir -p $(CTAGS_DIR); \
		pushd build/ctags/ && make install; popd; \
		$(LN) $(CTAGS_DIR)/bin/ctags $(HOME)/bins/default/bin/ctags && \
		$(LN) $(CTAGS_DIR)/bin/readtags $(HOME)/bins/default/bin/readtags; \
	fi
	if [ ! -f $(TMUX_DIR)/bin/tmux ]; then \
		mkdir -p $(TMUX_DIR); \
		pushd build/tmux && make install; popd; \
		$(LN) $(TMUX_DIR)/bin/tmux $(HOME)/bins/default/bin/tmux; \
	fi
	if [ ! -f $(NODE_DIR)/bin/node ]; then \
		pushd build/node_prebuilt && mv $(NODE_NAME) $(NODE_DIR); popd; \
	fi
	if [ ! -f $(HOME)/bins/default/bin/rg ]; then \
		mkdir -p $(HOME)/bins/default; \
		cp -r build/rg/$(RG_NAME)/rg $(HOME)/bins/default/bin/rg; \
	fi

config:  ## setup the configuration of modules
	# vim
	$(LN) $(CUR_DIR)/vim $(VIM_CONF)
	test -f $(VIM_CONF)/autoload/plug.vim || \
		curl -fkLo $(VIM_CONF)/autoload/plug.vim --create-dirs \
		https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	if [ ! -d vim/code_snippets ]; then \
		git submodule update --init --recursive; \
	fi
	# tmux
	test -L $(HOME)/.tmux.conf || $(LN) $(CUR_DIR)/tmux.conf $(HOME)/.tmux.conf
	# bash
	mkdir -p $(BASH_CONF)
	test -L $(BASH_CONF)/bashrc || $(LN) $(CUR_DIR)/bash/bashrc $(BASH_CONF)/bashrc
ifneq ($(shell uname -s),Darwin)
	# rime
	mkdir -p $(HOME)/.config/ibus/
	test -L $(HOME)/.config/ibus/rime || $(LN) $(CUR_DIR)/squirrel/ $(HOME)/.config/ibus/rime
endif

uninstall:  ## uninstall the modules
	# vim
	rm -rf $(VIM_DIR) $(VIM_CONF)
	rm -rf $(HOME)/bins/default/bin/{vim,xxd}
	# ctags
	rm -rf $(CTAGS_DIR)
	rm -rf $(HOME)/bins/default/bin/{ctags,readtags}
	# tmux
	rm -rf $(TMUX_DIR)
	rm -rf $(HOME)/bins/default/bin/tmux
	rm -rf $(HOME)/.tmux.conf
	# node
	rm -rf $(NODE_DIR)
	rm -rf $(HOME)/.npm/
	# rg
	rm -rf $(HOME)/bins/default/bin/rg
	# bash
	rm -rf $(BASH_CONF)
ifneq ($(shell uname -s),Darwin)
	# rime
	rm -rf $(HOME)/.config/ibus/rime
endif

clean:
	for i in $(shell ls build); do \
		make -C build/$$i clean distclean; \
	done
	make -C build/vim/ distclean
