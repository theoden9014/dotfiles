(add-to-list 'auto-mode-alist '("\\.js[x]?$" . web-mode))
(setq web-mode-content-types-alist
      '(("jsx" . "\\.js[x]?\\'")))

;; indent
(add-hook 'web-mode-hook
          '(lambda ()
             (setq web-mode-attr-indent-offset nil)
             (setq web-mode-markup-indent-offset 2)
             (setq web-mode-css-indent-offset 2)
             (setq web-mode-code-indent-offset 2)
             (setq web-mode-sql-indent-offset 2)
             (setq indent-tabs-mode nil)
             (setq tab-width 2)
          ))

;; color
;; (custom-set-faces
;;  '(web-mode-doctype-face           ((t (:foreground "#4A8ACA"))))
;;  '(web-mode-html-tag-face          ((t (:foreground "#4A8ACA"))))
;;  '(web-mode-html-attr-name-face    ((t (:foreground "#87CEEB"))))
;;  '(web-mode-html-attr-equal-face   ((t (:foreground "#FFFFFF"))))
;;  '(web-mode-html-attr-value-face   ((t (:foreground "#D78181"))))
;;  '(web-mode-comment-face           ((t (:foreground "#587F35"))))
;;  '(web-mode-server-comment-face    ((t (:foreground "#587F35"))))
;;
;;  '(web-mode-css-at-rule-face       ((t (:foreground "#DFCF44"))))
;;  '(web-mode-comment-face           ((t (:foreground "#587F35"))))
;;  '(web-mode-css-selector-face      ((t (:foreground "#DFCF44"))))
;;  '(web-mode-css-pseudo-class       ((t (:foreground "#DFCF44"))))
;;  '(web-mode-css-property-name-face ((t (:foreground "#87CEEB"))))
;;  '(web-mode-css-string-face        ((t (:foreground "#D78181"))))
;;  )
