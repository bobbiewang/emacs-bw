(setq package-user-dir (locate-user-emacs-file "var/elpa"))

(setq nsm-settings-file (locate-user-emacs-file "var/network-security.data"))

(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache (expand-file-name "var/eln-cache/" user-emacs-directory)))
