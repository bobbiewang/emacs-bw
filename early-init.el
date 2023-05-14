(setq package-user-dir (locate-user-emacs-file "var/elpa"))

(setq nsm-settings-file (locate-user-emacs-file "var/network-security.data"))

(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (expand-file-name "var/eln-cache/" user-emacs-directory)))
