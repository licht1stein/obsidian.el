;;; obsidian-ext.el --- Extention to obsidian.el -*- coding: utf-8; lexical-binding: t; -*-

(setq backlink-format "%-33s%-33s\n")


;; (switch-to-buffer "*scratch*")

;; (princ (obsidian--backlinks) (get-buffer "*scratch*"))

;; (print "jabroni" (get-buffer "*scratch*"))

;; (setq thing "\njabroni\n")
;; (princ "thing\n" (get-buffer "*scratch*"))
;; ;; (add-text-properties 1 3 '(comment t jabroni face) thing)
;; (princ (propertize "thing" 'face 'highlight) (get-buffer "*scratch*"))

;; (text-properties-at (point))


(defun mes ()
  "message me"
  (interactive)
  (message "%-20s%-20s"
           (propertize "link" 'face 'font-lock-string-face 'obsidian--link '(:a 1 :b 2))
           (propertize "explanation text" 'face 'font-lock-comment-face 'obsidian--link '(:a 3 :b 4))))

;; (with-current-buffer-window "*backlinks-buffer*"
;;     nil
;;     nil
;;   (progn
;;     (insert "In the time of chimpanzees there was a ")
;;     ;; (insert (propertize "jabroni" 'face 'markdown-link-face 'obsidian--links 'jabronski))
;;     (insert (propertize "jabroni" 'face 'font-lock-string-face 'obsidian--links 'jabronski))
;;     (insert ". Butane in my veins...")
;;     ;; (markdown-mode)
;;     (obsidian-mode t)))

;; (with-temp-buffer-window "*backlinks-buffer*"
;;     nil
;;     nil
;;   (progn
;;     (obsidian-mode)
;;     (print "jabroni")))


(defun link-with-props (k v)
  (let* ((txt (format backlink-format (obsidian--file-relative-name k) (nth 2 v)))
         (ptxt (propertize txt 'face 'font-lock-string-face 'obsidian--link-map v)))
    (insert ptxt)))

(defun obsidian-backlinks-other-window ()
  (interactive)
  (if (and obsidian-mode (obsidian--file-p))
      (let* ((backlinks (obsidian--backlinks))
             (base (file-name-base (buffer-file-name)))
             (buffer "*backlinks-buffer*"))
        (when backlinks
          ;; (with-output-to-temp-buffer buffer
          (with-current-buffer-window buffer
              nil
              nil
            (progn
              (insert (format backlink-format "File Name" "Link Text"))
              (insert "-----------------------------------------------------\n")
              (maphash 'link-with-props backlinks)
              (obsidian-mode t)))))))


;; (add-hook 'buffer-list-update-hook #'obsidian-backlinks-other-window)
;; (remove-hook 'buffer-list-update-hook #'obsidian-backlinks-other-window)

;;;###AUTOLOAD
(define-minor-mode obsidian-backlinks-mode
  "When active, open a buffer showing the backlinks for the current file.

Opening an Obsidian file will automatically create a separate
temporary buffer showing the backlinks to that file.

The backlinks themselves are links, linking back to the location pointed
in the linked file."
  :global t
  :lighter " Bk"
  (cond
   (obsidian-backlinks-mode
    ;; mode was turned on
    (add-hook 'buffer-list-update-hook #'obsidian-backlinks-other-window))
   (t
    ;; mode was turned off (or we refused to turn it on)
    (remove-hook 'buffer-list-update-hook #'obsidian-backlinks-other-window))))


(defun obsidian-indent-indent-buffer ()
  "Add indentation properties to the accessible part of the buffer."
  (interactive)
  (if (not t) ;; (derived-mode-p 'org-mode)
      (error "Not in Obsidian mode")
    (message "Setting buffer indentation.  It may take a few seconds...")
    (org-indent-remove-properties (point-min) (point-max))
    (org-indent-add-properties (point-min) (point-max))
    (message "Indentation of buffer set.")))

;; TODO: This docstring is taken right from org-indent-mode
;;;###autoload
(define-minor-mode markdown-indent-mode
  "When active, indent text according to outline structure.

Internally this works by adding `line-prefix' and `wrap-prefix'
properties, after each buffer modification, on the modified zone.

The process is synchronous.  Though, initial indentation of
buffer, which can take a few seconds on large buffers, is done
during idle time."
  :global t
  :lighter " MInd")


(global-set-key (kbd "<f7>") (lambda () (interactive) (message "%s" (text-properties-at (point)))))
(global-set-key (kbd "<f8>") 'obsidian-backlinks-mode)


(provide 'obsidian-ext)
;; obsidian-ext ends here
