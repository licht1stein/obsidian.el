(require 'dash)
(require 's)

(require 'cl-lib)
(require 'company)

;; Clojure style comment
(defmacro -comment (&rest body)
  "Ignores body, yields nil."
  nil)

(defcustom obsidian-directory nil
  "Path to Obsidian Notes vault."
  :group 'obsidian
  :type 'directory)

(setq obsidian-tags-list '())

(defun obsidian-specify-path (&optional path)
  "Specifies obsidian folder path to obsidian-folder variable. When run interactively asks user to specify the path."
  (interactive)
  (->> (or path (read-directory-name "Specify path to Obsidian folder"))
       (expand-file-name)
       (customize-set-value 'obsidian-directory)))

(-comment
 (obsidian-specify-path)
 (obsidian-specify-path "~/Sync/Zettelkasten/"))

;;; File utilities
;; Copied from org-roam's org-roam-descendant-of-p
(defun obsidian-descendant-of-p (a b)
  "Return t if A is descendant of B."
  (unless (equal (file-truename a) (file-truename b))
    (string-prefix-p (replace-regexp-in-string "^\\([A-Za-z]\\):" 'downcase (expand-file-name b) t t)
                     (replace-regexp-in-string "^\\([A-Za-z]\\):" 'downcase (expand-file-name a) t t))))

(defun obsidian-not-trash-p (file)
  "Returns t if file is not in .trash of Obsidian."
  (not (obsidian-descendant-of-p file (concat obsidian-directory ".trash"))))

(defun obsidian-file? (&optional file)
  "Return t if FILE is an obsidian.el file, nil otherwise.

If FILE is not specified, use the current buffer's file-path.
FILE is an Org-roam file if:
- It's located somewhere under `obsidian-directory
- It is a markdown .md file
- It is not in .trash
- It is not an Emacs temp file"
  (-when-let* ((path (or file (-> (buffer-base-buffer) buffer-file-name)))
	       (relative-path (file-relative-name path obsidian-directory))
	       (ext (file-name-extension relative-path))
	       (md? (string= ext "md"))
	       (obsidian-dir? (obsidian-descendant-of-p path obsidian-directory))
	       (trash-dir (concat obsidian-directory ".trash"))
	       (not-trash? (obsidian-not-trash-p path))
	       (not-temp? (not (s-contains? "~" relative-path))))
    t))

(defun obsidian-list-all-files ()
  "Lists all Obsidian Notes files that are not in trash.

Obsidian notes files:
- Pass the 'obsidian-file? check"
  (->> (directory-files-recursively obsidian-directory "\.*$")
       (-filter 'obsidian-file?)))

(-comment
 "#tag1 #tag2"

 (setq sample-file "~/Sync/Zettelkasten/Literature/Самадхи у Кинга.md")
 (obsidian-descendant-of-p sample-file obsidian-directory) ;; => t
 (obsidian-file?)					   ;; => nil
 (obsidian-file? "~/Sync/Zettelkasten/Literature/Самадхи у Кинга.md") ;; => t
 (obsidian-file? "~/Sync/Zettelkasten/.trash/2021-10-26.md") ;; => nil
 )

(defun obsidian-read-file-or-buffer (&optional file)
  "Returns string contents of a file or current buffer.

If FILE is not specified, use the current buffer."
  (if file
      (with-temp-buffer
	(insert-file-contents file)
	(buffer-substring-no-properties (point-min) (point-max)))
    (buffer-substring-no-properties (point-min) (point-max))))

(setq obsidian--tag-regex "#[[:alnum:]-_=+]+")
(defun obsidian-find-tags (s)
  "Finda all #tags in string."
  (->> (s-match-strings-all obsidian--tag-regex s)
       -flatten))

(defun obsidian-tag? (s)
  "Returns t if s matches obsidian--tag-regex, else nil."
  (when (s-match obsidian--tag-regex s)
    t))

(-comment
 (obsidian-tag? "#foo"))

(defun obsidian-find-tags-in-file (&optional file)
  "Returns all tags in file or current buffer.

If FILE is not specified, use the current buffer"
  (-> (obsidian-read-file-or-buffer file)
      obsidian-find-tags
      -distinct))

(defun obsidian-list-all-tags ()
  "Finds all tags in all obsidian files."
  (->> (obsidian-list-all-files)
       (mapcar 'obsidian-find-tags-in-file)
       -flatten
       -distinct))

(-comment
 (obsidian-read-file-or-buffer)
 (obsidian-read-file-or-buffer sample-file)
 (obsidian-find-tags "foo #foo # #тэг-такой spam") ;; => ("#foo" "#тэг-такой")
 (obsidian-find-tags-in-file)
 (obsidian-find-tags-in-file sample-file)
 (obsidian-list-all-tags))

(defun obsidian-update-tags-list ()
  "Scans entire Obsidian vault and updates all tags for completion."
  (interactive)
  (->> (obsidian-list-all-tags)
       (setq obsidian-tags-list)))

(-comment
 (obsidian-update-tags-list))

(define-minor-mode obsidian-mode
  "Toggle minor obsidian-mode on and off.

Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state."
  ;; The initial value.
  :init-value nil
  :lighter "obs"
  :after-hook (obsidian-update-tags-list))

(defun obsidian-tags-backend (command &optional arg &rest ignored)
  "company-mode backend for obsidian.el."
  (interactive (list 'interactive))

  (cl-case command
    (interactive (company-begin-backend 'obsidian-tags-backend))
    (prefix (when (and
		   (-contains? local-minor-modes 'obsidian-mode)
		   (looking-back obsidian--tag-regex))
	      (match-string 0)))
    (candidates (->> obsidian-tags-list
		     (-filter (lambda (s) (s-starts-with? arg s t)))))))

(add-to-list 'company-backends 'obsidian-tags-backend)

(defun obsidian-enable-minor-mode ()
  "Check if current buffer is an obsidian-file? and if it is enable minor obsidian-mode."
  (when (obsidian-file?)
    (obsidian-mode t)))

(add-hook 'markdown-mode-hook #'obsidian-enable-minor-mode)

(provide 'obsidian)
