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
(let ((default-directory (expand-file-name "~/.emacs.d/mode")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))
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

(defun install-my-packages ()
  "install my packages"
  (interactive)
  (package-refresh-contents)
  (package-install 'json-mode)
  (package-install 'auto-complete)
  (package-install 'yaml-mode)
  (package-install 'terraform-mode)
  (package-install 'web-mode)
  (package-install 'haml-mode)
  (package-install 'dockerfile-mode)
  (package-install 'go-mode)
  (package-install 'go-autocomplete)
  (package-install 'go-imports)
  (package-install 'golint)
  (package-install 'go-tag))


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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (js2-mode)))
 '(safe-local-variable-values (quote ((c-indent-level . 4)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;--------------------------------------------------------------------------------
;; Emacs configuration
;;--------------------------------------------------------------------------------
;; Don't Create Backup files
;; (*.~ , .#*))
(setq make-backup-files nil)
(setq auto-save-default nil)


;; Delete trailing whitespace when save file
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (go-tag golint go-imports go-autocomplete go-mode dockerfile-mode haml-mode web-mode terraform-mode yaml-mode auto-complete json-mode esup js2-mode)))
 '(safe-local-variable-values (quote ((c-indent-level . 4))))
 '(terraform-indent-level 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Show Hilight
(show-paren-mode t)
(setq show-paren-delay 0) ; Time to bedisplay (sec)
