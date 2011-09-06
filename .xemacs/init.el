;; Load libraries
;; ==============
(setq load-path (cons 
                 (expand-file-name "~/.xemacs/elisp/") 
                 load-path
                 ))

(turn-on-font-lock)

(add-hook 'lisp-mode-hook       'turn-on-font-lock)
(add-hook 'emacs-lisp-mode-hook 'turn-on-font-lock)
(add-hook 'dired-mode-hook      'turn-on-font-lock)
(add-hook 'font-lock-mode-hook  'turn-on-fast-lock)
(add-hook 'sml-mode-hook        'turn-on-font-lock)
(add-hook 'diff-mode-hook       'turn-on-font-lock)
(add-hook 'python-mode-hook     'turn-on-font-lock)
(add-hook 'lua-mode-hook        'turn-on-font-lock)

(when (featurep 'mule)
  ;; Mule-UCS-Unicode for emacsen 20.x and 21.x
  (when (and (>= emacs-major-version 20)
             (<= emacs-major-version 21))
    (if (fboundp 'un-define-debian-latin)
        (un-define-debian-latin)
      (if (locate-library "un-define")
          (require 'un-define))))
  (let ((case-fold-search t)
        locale vars cs)
    (setq vars '("LC_ALL" "LC_CTYPE" "LANG"))
    (while (and vars (not (setq locale (getenv (car vars)))))
      (setq vars (cdr vars)))
    (or locale (setq locale "C"))
    (when (string-match "UTF-?8" locale)
      (setq cs 'utf-8)
      (prefer-coding-system cs)
      (set-keyboard-coding-system cs)
      (set-terminal-coding-system cs))))

;;; func-menu is a package that scans your source file for function definitions
;;; and makes a menubar entry that lets you jump to any particular function
;;; definition by selecting it from the menu.  The following code turns this on
;;; for all of the recognized languages.  Scanning the buffer takes some time,
;;; but not much.
;;;
(cond ((string-match "XEmacs\\|Lucid" emacs-version)
       (require 'func-menu)
       (define-key global-map 'f8 'function-menu)
       (add-hook 'find-file-hooks 'fume-add-menubar-entry)
       (define-key global-map "\C-cg" 'fume-prompt-function-goto)
       (define-key global-map '(shift button3) 'mouse-function-menu)
 
       (setq fume-max-items 25
             fume-fn-window-position 3
             fume-auto-position-popup t
             fume-display-in-modeline-p t
             fume-menubar-menu-location "File"
             fume-buffer-name "*Function List*"
             fume-no-prompt-on-valid-default nil)

       ))

;; Set some variables
(setq find-file-use-truenames nil
      find-file-compare-truenames t
      minibuffer-confirm-incomplete t
      next-line-add-newlines nil
      complex-buffers-menu-p t
      minibuffer-max-depth nil
      default-input-method 'cyrillic-jcuken

      ;; Change the cursor used when the mouse is over a mode line
      x-mode-pointer-shape "leftbutton"

      ;;remove the next comments to prevent loading default libraries
      ;(setq-default inhibit-default-init 1)
      mail-yank-prefix "> ")


;; Change the cursor used during garbage collection.
(if (featurep 'xpm)
    (setq x-gc-pointer-shape
          (expand-file-name "recycle.xpm" data-directory)))

;; FIXME: to be redefined
;; Buffers
(define-key global-map 'f12       'switch-to-other-buffer)
(define-key global-map 'f11       'bury-buffer)
(define-key global-map "\C-x\C-k" 'kill-this-buffer)

(define-key global-map [(control button4)]
  '(lambda (&rest args)
     (interactive "_") 
     (let ((curwin (selected-window)))
       (select-window (car (mouse-pixel-position)))
       (scroll-down)
       (select-window curwin))))

(define-key global-map [(control button5)]
  '(lambda (&rest args)
     (interactive "_") 
     (let ((curwin (selected-window)))
       (select-window (car (mouse-pixel-position)))
       (scroll-up)
       (select-window curwin))))

(require 'tcl)
(require 'cc-langs)
(require 'cc-styles)
(require 'cc-vars)
(require 'cperl-mode)

;(require 'bbdb)
;(setq bbdb/gnus-mark-known-posters nil)

;; art's config
(global-unset-key 'insert)
(global-set-key '(meta space) 'set-mark-command)
(global-set-key '(meta ??)       'comment-region)

(load-library '"make-mode")
(define-key makefile-mode-map '$ 'makefile-insert-macro-ref)

;;; ********************
;;; Load the auto-save.el package, which lets you put all of your autosave
;;; files in one place, instead of scattering them around the file system.
;;;
(require 'auto-save)

(put 'narrow-to-region 'disabled nil)

;; Other settings
;;================
(set-variable 'default-tab-width '4)
(set-variable 'bar-cursor 'nil)

(require 'info)
(setq Info-suffix-list
      (append '(
                (".info.bz2" . "bzip2 -dc %s")
                (".bz2"      . "bzip2 -dc %s")
                )
              Info-suffix-list))

(require 'tex-site)

(gnuserv-start)
(display-time)

; mailcrypt
;;(load-library "mailcrypt") ; provides "mc-setversion"
;;(mc-setversion "gpg")    ; for PGP 2.6 (default); also "5.0" and "gpg"

;(autoload 'mc-install-write-mode "mailcrypt" nil t)
;(autoload 'mc-install-read-mode "mailcrypt" nil t)
;(add-hook 'mail-mode-hook 'mc-install-write-mode)

;; settings for folding-mode
(autoload 'folding-mode          "folding" "Folding mode" t)
(autoload 'turn-off-folding-mode "folding" "Folding mode" t)
(autoload 'turn-on-folding-mode  "folding" "Folding mode" t)

;;;
;;; VHDL mode
;;;
(autoload 'vhdl-mode "vhdl-mode" "VHDL Mode" t)
(setq auto-mode-alist (cons '("\\.vhdl?\\'" . vhdl-mode) auto-mode-alist))

;;;
;;; SML mode
;;;
(setq sml-mode-info "/usr/local/share/emacs/site-lisp/sml-mode/sml-mode.info")
(setq load-path (cons "/usr/local/share/emacs/site-lisp/sml-mode/" load-path))

(require 'sml-mode)
;(require 'sml-font)

(setq sml-type-of-indent nil
      sml-nested-if-indent t)

(setq sml-op-prec
      (append sml-op-prec 
              '(
                ("@-@" .  5) ("\\\\" . 5) ("@@"   . 5) ("@\\" . 5) ("^^" . 5)
                ("oo"  .  3) ("ooo"  . 3) ("oooo" . 3)
                ("$"   .  2) ("<$"   . 2) ("$>"   . 2) 
                ("ASSERT" . 2) ("ASSERT2" . 2) 
                ("ASSERT_fst2" . 2) ("ASSERT_snd2" . 2)
                ("WHEN" . 2) ("UNLESS" . 2) ("OTHERWISE" . 2) ("Option_WHEN" . 2)
                ("R_BIND" . 2) ("R_MAP" . 2) ("S_BIND" . 2) ("S_MAP" . 2)
                ("SS_BIND" . 2) ("SS_MAP" . 2) ("SSS_BIND" . 2) ("SSS_MAP" . 2) 
                ("RSS_BIND" . 2) ("RSS_MAP" . 2)
                ("TRACE" . 1) ("TRACE_FALSE" . 1)  
                ("BEFORE" . 0)  ("!\\" . 0) ("\\" . 0)
                ("<*>" . 7) ("<*?>" . 7) ("~<*>" . 7) ("<*>~" . 7)
                ("<+>" . 4)
                ("/+/" . 5) ("/-/" . 5)
                ("L_BIND" . 2)  ("P_BIND" . 2)  ("EL_BIND" . 2) ("EP_BIND" . 2)
                ("L_RBIND" . 2) ("L_LBIND" . 2) ("P_RBIND" . 2) ("P_LBIND" . 2)
                ("EL_BIND_L" . 2) ("L_BIND_EL" . 2)
                ("L_PLUS" . 1) ("P_PLUS" . 1) ("EL_PLUS" . 1) ("EP_PLUS" . 1)
                ("orFn" . 3) ("andFn" . 5)
                ("ENTUPLE" . 2)
                )))
