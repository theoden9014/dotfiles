;;--------------------------------------------------------------------------------
;; C mode 2014/02/27
;;--------------------------------------------------------------------------------

(defun add-c-mode-conf ()
  (c-set-style "k&r")
  (local-set-key (kbd "RET") 'newline-and-indent)
  )
(add-hook 'c-mode-hook 'add-c-mode-conf)

;;--------------------------------------------------------------------------------
