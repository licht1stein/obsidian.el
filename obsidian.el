(require 'dash)

(defcustom obsidian-path nil
  "Path to Obsidian Notes vault."
  :group 'obsidian
  :type 'directory)

;; Clojure style comment
(defmacro -comment (&rest body)
  "Ignores body, yields nil."
  nil)

(defun obsidian-specify-path (&optional path)
  "Specifies obsidian folder path to obsidian-folder variable. When run interactively asks user to specify the path."
  (interactive)
  (->> (or path (read-directory-name "Specify path to Obsidian folder"))
       (expand-file-name)
       (setq obsidian-path)))

(-comment
 (setq obsidian-path nil)
 (obsidian-specify-path)
 (obsidian-specify-path "~/Sync/Zettelkasten/"))
