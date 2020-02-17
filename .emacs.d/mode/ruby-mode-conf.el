;;--------------------------------------------------------------------------------
;; Ruby mode 2014/03/25
;;--------------------------------------------------------------------------------

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


                                        ;(require 'ruby-electric)
                                        ;(add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))
                                        ;(setq ruby-electric-expand-delimiters-list nil)

;; ruby-block.el --- highlight matching block
                                        ;(require 'ruby-block)
                                        ;(ruby-block-mode t)
                                        ;(setq ruby-block-highlight-toggle t)

;;--------------------------------------------------------------------------------
