;;--------------------------------------------------------------------------------
;; Diff mode
;;--------------------------------------------------------------------------------
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

;;--------------------------------------------------------------------------------
