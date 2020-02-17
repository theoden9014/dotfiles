(defun add-go-mode-conf ()
  (add-hook 'before-save-hook 'gofmt-before-save))
(add-hook 'go-mode-hook 'add-go-mode-conf)
