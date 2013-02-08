(global-set-key (quote [M-right]) (quote forward-sexp))
(global-set-key (quote [M-left]) (quote backward-sexp))
(global-set-key (quote [M-up]) (quote backward-paragraph))
(global-set-key (quote [M-down]) (quote forward-paragraph))

(global-set-key (quote [f11]) 'bury-buffer)

(load-file "~/.emacs.d/prev-next-buffer.el")
(global-set-key (quote [f12]) 'switch-to-other-buffer)

(global-unset-key (quote [insert]))
(global-set-key (quote "\M- ") 'set-mark-command)
(global-set-key (quote "\M-?") 'comment-region)

(global-set-key (quote [C-up]) (lambda () (interactive) (forward-line -6)))
(global-set-key (quote [C-down]) (lambda () (interactive) (forward-line 6)))

; NB: this is workaround: in the default keymap "C-x C-k" is a prefix key
(use-local-map ctl-x-map)
(local-set-key "\C-k" 'kill-this-buffer)
(use-global-map global-map)

(setq auto-mode-alist (cons '("\\.cu[h]?$" . c++-mode) (cons '("\\.\\(sdc\\|qsf\\|qip\\)$" . tcl-mode) auto-mode-alist)))

;;;
;;;

(menu-bar-mode (window-system))

(defun reverse-input-method (input-method)
  "Build the reverse mapping of single letters from INPUT-METHOD."
  (interactive
   (list (read-input-method-name "Use input method (default current): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)
        (modifiers '(nil (control) (meta) (control meta))))
    (when input-method
      (activate-input-method input-method))
    (when (and current-input-method quail-keyboard-layout)
      (dolist (map (cdr (quail-map)))
        (let* ((to (car map))
               (from (quail-get-translation
                      (cadr map) (char-to-string to) 1)))
          (when (and (characterp from) (characterp to))
            (dolist (mod modifiers)
              (define-key function-key-map
                (vector (append mod (list from)))
                (vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))

(reverse-input-method 'cyrillic-jcuken)  

(require 'color-theme)
(color-theme-initialize)
(color-theme-dark-laptop)
