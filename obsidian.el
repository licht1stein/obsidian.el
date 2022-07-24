(require 'dash)
(require 's)

;; Clojure style comment
(defmacro -comment (&rest body)
  "Ignores body, yields nil."
  nil)

(defcustom obsidian-directory nil
  "Path to Obsidian Notes vault."
  :group 'obsidian
  :type 'directory)

(defun obsidian-specify-path (&optional path)
  "Specifies obsidian folder path to obsidian-folder variable. When run interactively asks user to specify the path."
  (interactive)
  (->> (or path (read-directory-name "Specify path to Obsidian folder"))
       (expand-file-name)
       (setq obsidian-directory)))

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

(defun obsidian-file-p (&optional file)
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
- Pass the 'obsidian-file-p check"
  (->> (directory-files-recursively obsidian-directory "\.*$")
       (-filter 'obsidian-file-p)))

(-comment
 (setq sample-file "~/Sync/Zettelkasten/Literature/Самадхи у Кинга.md")
 (obsidian-descendant-of-p sample-file obsidian-directory) ;; => t
 (obsidian-file-p)					   ;; => nil
 (obsidian-file-p "~/Sync/Zettelkasten/Literature/Самадхи у Кинга.md")
 (->> (obsidian-file-p "~/Sync/Zettelkasten/Inbox/.Мои мысли об убийстве.md.~undo-tree~")
      (s-contains? "~"))
 (obsidian-file-p "~/Sync/Zettelkasten/.trash/2021-10-26.md") ;; => nil
 (directory-files obsidian-path)
 (obsidian-list-all-files)
 )
