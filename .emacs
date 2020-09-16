(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-include-dirs (quote ("/home/art/Build/agda-lib/lib-0.7/src" ".")))
 '(blink-cursor-mode nil)
 '(c++-mode-hook (quote ((lambda nil (setq c-basic-offset 4)))))
 '(c-basic-offset (quote set-from-style))
 '(c-default-style
   (quote
    ((c-mode . "linux")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu"))))
 '(c-mode-hook (quote ((lambda nil (setq c-basic-offset 4)))))
 '(calendar-week-start-day 1)
 '(column-number-mode t)
 '(display-time-24hr-format t)
 '(display-time-mode t)
 '(european-calendar-style t)
 '(fill-column 80)
 '(indent-tabs-mode nil)
 '(lua-indent-level 4)
 '(lua-mode-hook (quote ((lambda nil (setq indent-tabs-mode t)))))
 '(mouse-yank-at-point t)
 '(safe-local-variable-values (quote ((LaTeX-item-indent . -4) (LaTeX-indent-level . 4))))
 '(split-width-threshold nil)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(vhdl-basic-offset 4)
 '(vhdl-electric-mode nil)
 '(visible-bell t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-highlight-datatype-face ((t (:foreground "light blue"))))
 '(agda2-highlight-function-face ((t (:foreground "light blue"))))
 '(agda2-highlight-postulate-face ((t (:foreground "light blue"))))
 '(agda2-highlight-primitive-face ((t (:foreground "light blue"))))
 '(agda2-highlight-primitive-type-face ((t (:foreground "light blue"))))
 '(agda2-highlight-record-face ((t (:foreground "light blue"))))
 '(agda2-highlight-unsolved-constraint-face ((t (:background "orange3" :foreground "black"))))
 '(agda2-highlight-unsolved-meta-face ((t (:background "orange3" :foreground "black")))))

(load-file "~/.emacs.d/init.el")
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; (load-file (let ((coding-system-for-read 'utf-8))
;;                 (shell-command-to-string "agda-mode locate")))
