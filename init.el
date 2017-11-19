;; -*- coding: utf-8-unix -*-

;;; Code:

;; 初始化 ELPA 环境

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(setq package-user-dir (locate-user-emacs-file ".elpa"))
(setq package-enable-at-startup nil)    ; 不在 init 文件加载后重复初始化
(package-initialize)

;; 通过 Org Babel 加载 core 文件和 modules 文件

(require 'ob-tangle)

(defun bw/load-core-files ()
  "加载 core 配置文件."
  (interactive)
  (dolist (pkg '(infrastructure packages ui misc))
    (org-babel-load-file (locate-user-emacs-file (format "core/config-%s.org" pkg)))))

(defvar bw/modules-dir (locate-user-emacs-file "modules/"))
(defun bw/load-modules-files ()
  "加载 modules 配置文件."
  (interactive)
  (when (file-exists-p bw/modules-dir)
  (message "Loading configuration files of modules...")
  (mapc 'org-babel-load-file (directory-files bw/modules-dir 't "config-.*org$"))))

(bw/load-core-files)
(bw/load-modules-files)

(provide 'init)

;;; init.el ends here
