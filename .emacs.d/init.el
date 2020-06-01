
(require 'cl)

;;--------------------------------------------------------------------------------
;; Load Path
;;--------------------------------------------------------------------------------
(defvar darwin-p (eq system-type 'darwin))      ; for Mac OS X
(defvar nt-p (eq system-type 'windows-nt))      ; for Windows

;; This is load configuration for mode.
;; Load from Path variable.
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-loadpath)
            (normal-top-level-add-subdirs-to-load-path))))))

;; Set preference directory
(setq preferences-directory "~/.emacs.d/")

;; Branch OS
(add-to-load-path "config")
(if (eq system-type 'gnu/linux) (load "linux"))  ; Linux
(if (eq system-type 'windows-nt) (load "windows")) ; Windows
(if (eq system-type 'mac) (load "mac")) ; Mac

;; for GUI (window-system)
(when window-system (load "window-system"))

;; Don't show start display
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Load Configuration File for each Emacs Version
(defun load-file-in-dir (dir file)
  (load (concat dir file)))
(put 'narrow-to-region 'disabled nil)


;;--------------------------------------------------------------------------------
;; Package Manager
;;--------------------------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; check if each package is installed and install it if is not
(dolist (pkg '(use-package flycheck))
  (unless (package-installed-p pkg)
    (package-refresh-contents)
    (package-install pkg)
    ))

(eval-when-compile
   (require 'use-package))


;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))


(use-package flycheck)

(defvar installing-package-list
  '(
    json-mode
    auto-complete
    yaml-mode
    terraform-mode
    web-mode
    haml-mode
    dockerfile-mode
    go-mode
    lsp-mode

    use-package
    dap-mode
    which-key

    flycheck
    ))

(let ((not-installed (loop for x in installing-package-list
                           when (not (package-installed-p x))
                           collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
      (package-install pkg))))


;;--------------------------------------------------------------------------------
;; Shortcut
;;--------------------------------------------------------------------------------
;; Change key bind for Window Movement (Same to tmux)
(global-set-key (kbd "C-x <left>")  'windmove-left)
(global-set-key (kbd "C-x <down>")  'windmove-down)
(global-set-key (kbd "C-x <up>")    'windmove-up)
(global-set-key (kbd "C-x <right>") 'windmove-right)

;; Similar Vi
(global-set-key (kbd "C-l") 'right-char)
(global-set-key (kbd "C-h") 'left-char)

;; Change key bind for Split Window (Same to tmux)
(global-set-key (kbd "C-x %") 'split-window-right)
(global-set-key (kbd "C-x \"") 'split-window-below)

;;--------------------------------------------------------------------------------
;; Emacs configuration
;;--------------------------------------------------------------------------------
;; Don't Create Backup files
;; (*.~ , .#*))
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Delete trailing whitespace when save file
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Show Hilight
(show-paren-mode t)
(setq show-paren-delay 0) ; Time to bedisplay (sec)


;;--------------------------------------------------------------------------------
;; Package configuration
;;--------------------------------------------------------------------------------
;; auto-complete
(require 'auto-complete)
;(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20130724.1750/dict")
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)


;;--------------------------------------------------------------------------------
;; Language configuration
;;--------------------------------------------------------------------------------
;; c++-mode
(defun add-c++-mode-conf ()
  (c-set-style "stroustrup")
  (setq indent-tabs-mode nil)
  (c-set-offset 'innamespace 0)   ;; don't indent for "namespace {}"
  (c-set-offset 'arglist-close 0) ;; don't indent for closing parenthesis of the argument list of the function
  (local-set-key (kbd "RET") 'newline-and-indent)
  )
(add-hook 'c++-mode-hook 'add-c++-mode-conf)

;; c-mode
(defun add-c-mode-conf ()
  (c-set-style "k&r")
  (local-set-key (kbd "RET") 'newline-and-indent)
  )
(add-hook 'c-mode-hook 'add-c-mode-conf)

;; diff mode
;; ;; Change Display
;; (defun diff-mode-setup-faces ()
;;   ;; Show green if add rows
;;   (set-face-attribute 'diff-added nil
;;                    :foreground "white" :background "dark green")
;;   ;; Show red if delete rows
;;   (set-face-attribute 'diff-removed nil
;;                    :foreground "white" :background "dark red"))
;; (add-hook-fn 'diff-mode-hook (diff-mode-setup-faces))
(defun add-diff-mode-conf ()
  ;; Show green if add rows
  (set-face-attribute 'diff-added nil
                                            :foreground "white" :background "dark green")
  ;; Show red if delete rows
  (set-face-attribute 'diff-removed nil
                                            :foreground "white" :background "dark red")
  )
(add-hook 'diff-mode-hokk 'add-diff-mode-conf)

;; lsp-mode
(use-package lsp-mode
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  :init (setq lsp-keymap-prefix "s-l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (go-mode . lsp)
	 (go-mode . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)
	 )
  :config
  (setq lsp-enable-file-watchers t)
  (setq lsp-file-watch-threshold 5000)
  :commands (lsp lsp-deferred))

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)
;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
  :config
  (which-key-mode))

;; Optional - provides snippet support.
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

;; go-mode
(use-package go-mode
  :commands go-mode
  :mode (("\\.go?\\'" . go-mode))
  :defer t
  :config
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (add-hook 'before-save-hook 'lsp-format-buffer)
  (add-hook 'before-save-hook 'lsp-organize-imports))

;; json-mode
(defun add-json-mode-conf ()
  (setq json-reformat:indent-width 2)
  (setq js-indent-level 2)
  (setq json-reformat:pretty-string? nil))

(add-hook 'json-mode-hook 'add-json-mode-conf)

;; org-mode
(unless package-archive-contents    ;; Refresh the packages descriptions
  (package-refresh-contents))
(setq package-load-list '(all))     ;; List of packages to load
(unless (package-installed-p 'org)  ;; Make sure the Org package is
  (package-install 'org))           ;; installed, install it if not
(package-initialize)                ;; Initialize & Install Package
;; (setq org-...)                   ;; Your custom settings


;; perl-mode
(defun add-perl-mode-conf ()
  (c-set-offset 'case-label 4)
  (c-set-offset 'arglist-intro 4)
  (c-set-offset 'arglist-cont-nonempty 4)
  (c-set-offset 'arglist-close 0)
  (setq c-basic-offset 4     ;; basic indent value
        tab-width 4          ;; tab value
        indent-tabs-mode nil)'
  (c-toggle-auto-hungry-state 1)     ;; if input ';' , auto create a new line and indent
  ;;      (define-key c-mode-base-map "\C-m" 'newline-and-indent)    ;; if input "Return" key, auto create anew line and indent
  (local-set-key (kbd "RET") 'newline-and-indent)
  )
(add-hook 'perl-mode-hook 'add-perl-mode-conf)

;; php-mode
(add-hook 'php-mode-hook
	  (lambda ()
	    (c-set-offset 'case-label 4)
	    (c-set-offset 'arglist-intro 4)
	    (c-set-offset 'arglist-cont-nonempty 4)
	    (c-set-offset 'arglist-close 0)
	    (setq c-basic-offset 4 ;; basic indent value
		  tab-width 4 ;; tab value
		  indent-tabs-mode nil)'
	    (c-toggle-auto-hungry-state 1) ;; if input ';' , auto create a new line and indent
	    (define-key c-mode-base-map "\C-m" 'newline-and-indent) ;; if input "Return" key, auto create anew line and indent
	    (local-set-key (kbd "RET") 'newline-and-indent)))


;; ruby-mode
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$latex " . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Berksfile$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

(defun add-ruby-mode-conf ()
  (setq tab-width 2)
  (setq ruby-indent-level tab-width)
  (setq ruby-deep-indent-paren-style nil)
;  (define-key ruby-mode-map [return] 'ruby-reindent-then-newline-and-indent)))
  (define-key ruby-mode-map [return] 'reindent-then-newline-and-indent)
  )
(add-hook 'ruby-mode-hook 'add-ruby-mode-conf)

;; web-mode
(add-to-list 'auto-mode-alist '("\\.js[x]?$" . web-mode))
(setq web-mode-content-types-alist
      '(("jsx" . "\\.js[x]?\\'")))

;; indent
(add-hook 'web-mode-hook
          '(lambda ()
             (setq web-mode-attr-indent-offset nil)
             (setq web-mode-markup-indent-offset 2)
             (setq web-mode-css-indent-offset 2)
             (setq web-mode-code-indent-offset 2)
             (setq web-mode-sql-indent-offset 2)
             (setq indent-tabs-mode nil)
             (setq tab-width 2)
          ))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(web-mode-comment-face ((t (:foreground "#587F35"))))
 '(web-mode-css-at-rule-face ((t (:foreground "#DFCF44"))))
 '(web-mode-css-property-name-face ((t (:foreground "#87CEEB"))))
 '(web-mode-css-pseudo-class ((t (:foreground "#DFCF44"))))
 '(web-mode-css-selector-face ((t (:foreground "#DFCF44"))))
 '(web-mode-css-string-face ((t (:foreground "#D78181"))))
 '(web-mode-doctype-face ((t (:foreground "#4A8ACA"))))
 '(web-mode-html-attr-equal-face ((t (:foreground "#FFFFFF"))))
 '(web-mode-html-attr-name-face ((t (:foreground "#87CEEB"))))
 '(web-mode-html-attr-value-face ((t (:foreground "#D78181"))))
 '(web-mode-html-tag-face ((t (:foreground "#4A8ACA"))))
 '(web-mode-server-comment-face ((t (:foreground "#587F35")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (which-key dap-mode yasnippet yaml-mode web-mode use-package terraform-mode lsp-ui json-mode haml-mode go-mode flycheck dockerfile-mode company-lsp auto-complete))))
