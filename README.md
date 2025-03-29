---
title: 个人环境配置
---

# 依赖

* vim: 确保系统版本在 9.0 以上，或者直接安装 neovim.
* tmux: `apt-get install tmux`
* nodejs：官网下载
    - 解压至 `~/bins/node` 目录下
* ripgrep
    - 可联网机器直接 `apt-get install ripgrep`
    - 解压至 `~/bins/ripgrep/` 目录下，并在 `~/bins/default/bin` 下面创建链接。
* universal ctags
    - 可联网机器直接 `apt-get install universal-ctags`
    - 不联网服务器官网下载源码编译
* miniconda3 or anaconda3：官网下载预编译版本，如果是不联网的服务器，直接下载 anaconda3，否则可以使用 miniconda3 来节省空间。

# 安装配置文件

根据情况编辑 `./bash/bashrc` 中的配置，然后执行：`make config`
