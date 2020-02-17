;;--------------------------------------------------------------------------------
;; C++ mode 2014/02/27
;;--------------------------------------------------------------------------------

(defun add-c++-mode-conf ()
  (c-set-style "stroustrup")
  (setq indent-tabs-mode nil)
  (c-set-offset 'innamespace 0)   ;; don't indent for "namespace {}"
  (c-set-offset 'arglist-close 0) ;; don't indent for closing parenthesis of the argument list of the function
  (local-set-key (kbd "RET") 'newline-and-indent)
  )
(add-hook 'c++-mode-hook 'add-c++-mode-conf)

;;--------------------------------------------------------------------------------
