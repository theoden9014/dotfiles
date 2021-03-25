
;; (require 'cl)

;;--------------------------------------------------------------------------------
;; Load Path
;;--------------------------------------------------------------------------------
(defvar darwin-p (eq system-type 'darwin))      ; for Mac OS X
(defvar nt-p (eq system-type 'windows-nt))      ; for Windows

;; for GUI (window-system)
(when window-system (load "window-system"))

;; Don't show start display
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;;--------------------------------------------------------------------------------
;; Package Manager
;;--------------------------------------------------------------------------------
(setq package-archives
        '(("marmalade"   . "http://marmalade-repo.org/packages/")
          ("gnu"         . "http://elpa.gnu.org/packages/")
          ("org"         . "http://orgmode.org/elpa/")
          ("melpa"       . "https://melpa.org/packages/")
          ("melpa-stable" . "https://stable.melpa.org/packages/")))
;; disable automatic loading of packages after the init file
(setq package-enable-at-startup nil)
;; instead load them explicitly
(package-initialize)
;; refresh package descriptions
(unless package-archive-contents
   (package-refresh-contents))

;;; use-package initialization
;;; install use-package if not already done
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
;;; use-package for all others
(require 'use-package)


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
;; Language configuration
;;--------------------------------------------------------------------------------
;; Go
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred))
;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)
;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  :config
  ;; Optionally enable completion-as-you-type behavior.
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))
;; Optional - provides snippet support.
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))


;; Ruby
(use-package ruby-mode
  :ensure t
  :commands ruby-mode
  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(dockerfile-mode yaml-mode protobuf-mode ruby-mode yasnippet use-package org lsp-ui company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Protocol Buffers
(use-package protobuf-mode
  :ensure t
  )

;; YAML
(use-package yaml-mode
  :ensure t
  )
