;; 缺省 800K 就进行 GC。
;; - 启动时设置为最大值，优化启动速度
;; - 启动完成后修改为 100M，减少卡顿
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 100000000)))

(setq package-user-dir (locate-user-emacs-file "var/elpa"))

(setq nsm-settings-file (locate-user-emacs-file "var/network-security.data"))

(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache (expand-file-name "var/eln-cache/" user-emacs-directory)))
