SHELL := /bin/bash
LN := ln -sfi
CUR_DIR := $(shell pwd)

VIM_DIR := $(HOME)/bins/vim9/
VIM_CONF := $(HOME)/.vim
CTAGS_DIR := $(HOME)/bins/ctags/
NODE_DIR := $(HOME)/bins/node/
BASH_CONF := $(HOME)/.config/bash/

RG_VER ?= 14.1.0
NODE_VER ?= v16.20.2
TMUX_VER ?= 3.3a
CTAGS_VER ?= 2024.07.02
CTAGS_BUILD_ID ?= 7cc5308a41787419fd08a73196aeb05672687c66
ifeq ($(shell uname -s),Darwin)
RG_ARCH ?= x86_64-apple-darwin
NODE_ARCH ?= darwin-x64
CTAGS_ARCH ?= macos-12.0-x86_64
INSTALLER := brew install
DEP_PKGS := python3 tmux ncurses
else
RG_ARCH ?= x86_64-unknown-linux-musl
NODE_ARCH ?= linux-x64
CTAGS_ARCH ?= linux-x86_64
INSTALLER := apt-get install -y
DEP_PKGS := curl libncurses-dev
endif
RG_NAME := ripgrep-$(RG_VER)-$(RG_ARCH)
RG_URL := https://github.com/BurntSushi/ripgrep/releases/download/$(RG_VER)/$(RG_NAME).tar.gz
NODE_NAME := node-$(NODE_VER)-$(NODE_ARCH)
NODE_URL := https://nodejs.org/dist/$(NODE_VER)/$(NODE_NAME).tar.gz
TMUX_URL := https://github.com/nelsonenzo/tmux-appimage/releases/download/$(TMUX_VER)/tmux.appimage
CTAGS_NAME := uctags-$(CTAGS_VER)-$(CTAGS_ARCH)
CTAGS_HOST := https://github.com/universal-ctags/ctags-nightly-build/
CTAGS_URL := $(CTAGS_HOST)/releases/download/$(CTAGS_VER)%2B$(CTAGS_BUILD_ID)/$(CTAGS_NAME).tar.gz

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
	if [ ! -d build/ctags_prebuilt/ ]; then \
		mkdir -p build/ctags_prebuilt; \
		curl -kL $(CTAGS_URL) -o build/ctags_prebuilt/uctags.tar.gz; \
	fi
	if [ ! -d build/tmux_prebuilt/ ]; then \
		mkdir -p build/tmux_prebuilt; \
		curl -kL $(TMUX_URL) -o build/tmux_prebuilt/tmux.appimage; \
	fi
	if [ ! -d build/node_prebuilt/ ]; then \
		mkdir -p build/node_prebuilt; \
		curl -kL $(NODE_URL) -o build/node_prebuilt/node.tar.gz; \
	fi
	if [ ! -d build/rg/ ]; then \
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
	if [ ! -f build/ctags_prebuilt/$(CTAGS_NAME)/bin/ctags ]; then \
		pushd build/ctags_prebuilt/ && \
		tar xvf uctags.tar.gz; \
		popd; \
	fi
	if [ ! -f build/tmux_prebuilt/tmux.appimage ]; then \
		pushd build/tmux_prebuilt/ && \
		(chmod +x tmux.appimage && mkdir -p $(HOME)/bins/default/bin/); \
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
		test -L $(HOME)/bins/default/bin/vim || \
			$(LN) $(VIM_DIR)/bin/vim $(HOME)/bins/default/bin/vim; \
		test -L $(HOME)/bins/default/bin/xxd || \
			$(LN) $(VIM_DIR)/bin/xxd $(HOME)/bins/default/bin/xxd; \
	fi
	if [ ! -f $(CTAGS_DIR)/bin/ctags ]; then \
		pushd build/ctags_prebuilt && mv $(CTAGS_NAME) $(CTAGS_DIR); popd; \
		test -L $(HOME)/bins/default/bin/ctags || \
			$(LN) $(CTAGS_DIR)/bin/ctags $(HOME)/bins/default/bin/ctags; \
		test -L $(HOME)/bins/default/bin/readtags || \
			$(LN) $(CTAGS_DIR)/bin/readtags $(HOME)/bins/default/bin/readtags; \
	fi
	if [ ! -f $(HOME)/bins/default/bin/tmux ]; then \
		mkdir -p $(HOME)/bins/default; \
		cp -r build/tmux_prebuilt/tmux.appimage $(HOME)/bins/default/bin/tmux && \
		chmod +x $(HOME)/bins/default/bin/tmux; \
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
	test -L $(VIM_CONF) || $(LN) $(CUR_DIR)/vim $(VIM_CONF)
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
	@echo '需要手动在 $(BASH_CONF)/bashrc 在 `~/.bashrc` 中增加 `source` 语句'
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
	make -C build/vim/ clean distclean
	rm -rf build/ctags_prebuilt/$(CTAGS_NAME)
	rm -rf build/node_prebuilt/$(NODE_NAME)
	rm -rf build/rg/$(RG_NAME)
