---
title: 个人环境配置
---

# Install

安装方式：`make download build install`，编辑 `bash/bashrc`中的配置后，执行 `make config` 安装配
置文件。

# Manual Install

如果需要根据情况手动安装，可以参考以下命令。

## bash

相应修改 `bash/bashrc` 中的配置，然后在 `~/.bashrc` 中增加 source 指向 `./bash/bashrc`

## tmux

tmux 编译的依赖较多，过于麻烦，直接使用包管理器安装。

配置：

```bash
ln -s $(realpath tmux.conf) $HOME/.tmux.conf
```

## vim

如果仓库中的版本大于 9.0，可以直接 `apt-get install vim`，否则需要编译安装：

```bash
./configure --with-features=huge --disable-gui --enable-python3interp --enable-multibyte \
    --prefix=$(VIM_DIR)
make -j4 && make install
```

配置方面需要先下载 code snippets，然后再准备好 vim-plug：

```bash
cd vim/code_snippets; git submodule update --init --recursive; cd -
ln -sfi $(realpath vim) $HOME/.vim/
curl -fkLo $HOME/.vim/autoload/plug.vim --create-dirs \
		https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
```

然后分别打开 vim 和 nvim 执行 `PlugInstall` 安装插件。

## universal ctags

可以下载安装 universal-ctags 的预编译版本，下载地址：
<https://github.com/universal-ctags/ctags-nightly-build/>

## node

主要 vim 中的 coc 使用，官网下载，解压后放到 `~/bins/node` 目录下。

## ripgrep

安装直接通过包管理软件安装或者下载预编译的二进制，如果是后者，而接近解压后放到 `~/bins/ripgrep/`
目录下，并在 `~/bins/default/bin` 下创建链接。

## Python

一般不使用系统自带的 python3 环境，而是自己安装 Miniconda 以便可以灵活的调整所需要的版本，而不会
和系统的 Python 版本冲突。

如果是离线环境，最好使用 Anaconda。

## docker

Debian 下直接 `apt-get install docker.io` 安装 docker，然后编辑配置文件 `/etc/docker/daemon.json`：

```json
{
    "registry-mirrors": [
        "https://registry.docker-cn.com",
        "https://docker.m.daocloud.io/",
        "https://huecker.io/",
        "https://dockerhub.timeweb.cloud",
        "https://noohub.ru/",
        "https://dockerproxy.com",
        "https://docker.mirrors.ustc.edu.cn",
        "https://docker.nju.edu.cn",
        "https://xx4bwyg2.mirror.aliyuncs.com",
        "http://f1361db2.m.daocloud.io",
        "https://registry.docker-cn.com",
        "http://hub-mirror.c.163.com",
        "https://docker.mirrors.ustc.edu.cn",
	    "https://docker.rainbond.cc/"
    ]
}
```

重启 docker：

```bash
systemctl daemon-reload
systemctl restart docker
```
