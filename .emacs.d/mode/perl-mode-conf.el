;;--------------------------------------------------------------------------------
;; Perl mode 2014/03/13
;;--------------------------------------------------------------------------------

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

;;--------------------------------------------------------------------------------
