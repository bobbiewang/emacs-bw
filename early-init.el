;; -*- lexical-binding: t -*-

;; 缺省 800K 就进行 GC。
;; - 启动时设置为最大值，优化启动速度
;; - 启动完成后修改为 100M，减少 GC 造成的卡顿
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 100000000)))

;; 设置 ELPA 路径
(let* ((env (getenv "XDG_DATA_HOME"))
       (data-home (if (or (null env)
                          (not (file-name-absolute-p env)))
                      (expand-file-name "~/.local/share")
                    env)))
  (setq package-user-dir
        (expand-file-name "emacs/elpa" data-home)))

;; 设置 eln-cache 路径
(when (boundp 'native-comp-eln-load-path)
  (let* ((env (getenv "XDG_CACHE_HOME"))
         (cache-home (if (or (null env)
                             (not (file-name-absolute-p env)))
                         (expand-file-name "~/.cache")
                       env)))
    (if (version< emacs-version "29")
        (add-to-list 'native-comp-eln-load-path eln-load-path)
      (startup-redirect-eln-cache
       (expand-file-name "emacs/eln-cache" cache-home)))))

;; 关闭 Native Compilation 的警告、错误消息
(setq native-comp-async-report-warnings-errors nil)
