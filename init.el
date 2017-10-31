;; -*- coding: utf-8-unix -*-

;; 初始化 ELPA 环境
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(setq package-user-dir (locate-user-emacs-file ".elpa"))
(setq package-enable-at-startup nil)    ; 不在 init 文件加载后重复初始化
(package-initialize)

;; 通过 Org Babel 加载 core 文件
(require 'ob-tangle)
(dolist (pkg '(infrastructure packages ui misc keybindings))
  (org-babel-load-file (locate-user-emacs-file (format "core/config-%s.org" pkg))))

;; 通过 Org Babel 加载 modules 文件
(setq bw/modules-dir (locate-user-emacs-file "modules/"))
(when (file-exists-p bw/modules-dir)
  (message "Loading configuration files of modules...")
  (mapc 'org-babel-load-file (directory-files bw/modules-dir 't "config-.*org$")))
