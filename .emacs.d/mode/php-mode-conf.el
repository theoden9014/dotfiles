;;--------------------------------------------------------------------------------
;; PHP mode 2014/02/25
;;--------------------------------------------------------------------------------
;; (add-hook 'php-mode-hook
;;           (lambda ()
;;             ;;            ;; auto indent when push enter key
;;             ;;            (defun ywb-php-lineup-arglist-intro (langelem)
;;             ;;              (save-excursion
;;             ;;                (goto-char (cdr langelem))
;;             ;;                (vector (+ (current-column) c-basic-offset))))
;;             ;;            (defun ywb-php-lineup-arglist-close (langelem)
;;             ;;              (save-excursion
;;             ;;                (goto-char (cdr langelem))
;;             ;;                (vector (current-column))))
;;             ;;            (c-set-offset 'arglist-intro 'ywb-php-lineup-arglist-intro)
;;             ;;            (c-set-offset 'arglist-close 'ywb-php-lineup-arglist-close)
;;             (c-set-offset 'case-label' 4)
;;             (c-set-offset 'arglist-intro' 4)
;;             (c-set-offset 'arglist-cont-nonempty' 4)
;;             (c-set-offset 'arglist-close' 0)
;;             (setq-default 'c-basic-offset' 4     ;; basic indent value
;;                           'tab-width' 4          ;; tab value
;;                           'indent-tabs-mode' nil)'
;;             (c-toggle-auto-hungry-state 1)     ;; if input ';' , auto create a new line and indent
;;             ;;           (define-key c-mode-base-map "\C-m" 'newline-and-indent)    ;; if input "Return" key, auto create anew line and indent
;;             (local-set-key (kbd "RET") 'newline-and-indent)
;;             ))

(defun add-php-mode-conf ()
;  (c-set-style "stroustrup")
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
(add-hook 'php-mode-hook 'add-php-mode-conf)

;;--------------------------------------------------------------------------------
