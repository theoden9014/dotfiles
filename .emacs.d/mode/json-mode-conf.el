;;--------------------------------------------------------------------------------
;; Json mode 2015/11/19
;;--------------------------------------------------------------------------------

(defun add-json-mode-conf ()
  (setq json-reformat:indent-width 2)
  (setq js-indent-level 2)
  (setq json-reformat:pretty-string? nil))

(add-hook 'json-mode-hook 'add-json-mode-conf)

