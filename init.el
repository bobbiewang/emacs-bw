;; -*- lexical-binding: t -*-
;; -*- coding: utf-8-unix -*-

;;; Code:

;; 初始化 ELPA 环境

(require 'package)

(defvar bw/package-source '163
  "Package 源，可选 official, 163, tuna 和 emacs-china")

(cond
 ((eq bw/package-source 'official)
  (setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/"))))
 ((eq bw/package-source '163)
  (setq package-archives '(("gnu"   . "http://mirrors.163.com/elpa/gnu/")
                           ("melpa" . "http://mirrors.163.com/elpa/melpa/"))))
 ((eq bw/package-source 'tuna)
  (setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                           ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))))
 ((eq bw/package-source 'emacs-china)
  (setq package-archives '(("gnu"   . "http://1.15.88.122/gnu/")
                           ("melpa" . "http://1.15.88.122/melpa/")))))

;; 不在 init 文件加载后重复初始化
(setq package-enable-at-startup nil)
(package-initialize)

(require 'xdg)

;; 将 site-lisp 下的包加到 load-path
;; 忽略 .git、.svn、RCS、CVS 等目录，以及包含 .nosearch 文件的目录
(defvar bw/site-lisp-dir (expand-file-name "site-lisp/" (xdg-data-home)))
(if (and (fboundp 'normal-top-level-add-subdirs-to-load-path)
         (file-exists-p bw/site-lisp-dir))
    (let* ((default-directory bw/site-lisp-dir)
           (orig-load-path load-path))
      (setq load-path (list default-directory))
      (normal-top-level-add-subdirs-to-load-path)
      (setq load-path (append load-path orig-load-path))))

;; 通过 Org Babel 加载 core 文件和 modules 文件

(require 'ob-tangle)

(defvar bw/core-dir (locate-user-emacs-file "core/"))
(defun bw/load-core-files ()
  "加载 core 配置文件."
  (interactive)
  (message "Loading core configuration files...")
  (dolist (pkg '(infrastructure packages ui misc))
    (org-babel-load-file (format "%sconfig-%s.org" bw/core-dir pkg))))

(defvar bw/modules-dir (locate-user-emacs-file "modules/"))
(defun bw/load-modules-files ()
  "加载 modules 配置文件."
  (interactive)
  (when (file-exists-p bw/modules-dir)
    (message "Loading configuration files of modules...")
    (mapc 'org-babel-load-file
          (directory-files bw/modules-dir 't "config-.*org$"))))

(bw/load-core-files)
(bw/load-modules-files)

(provide 'init)

;;; init.el ends here
