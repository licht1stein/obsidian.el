;;; obsidian.el --- An Emacs interface for Obsidian Notes -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (c) 2022 Mykhaylo Bilyanskyy <mb@blaster.ai>

;; Author: Mykhaylo Bilyanskyy
;; URL: https://github.com./licht1stein/obsidian.el
;; Keywords: obsidian, pkm, convenience
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1") (company "0.9.13") (s "20210616.619") (dash "2.13"))

;; This file is NOT part of GNU Emacs.

;;; License:
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:
;;
;; obsidian.el lets you interact with more convenience with markdown files
;; that are contained in Obsidian Notes vault.  It adds autocompletion for
;; tags and links, jumping between notes, capturing new notes into inbox etc.

;;; Code:
(require 'dash)
(require 's)

(require 'cl-lib)
(require 'company)

;; Clojure style comment
(defmacro -comment (&rest _)
  "Ignore body, yield nil."
  nil)

(-comment
 (package-buffer-info))

(defcustom obsidian-directory nil
  "Path to Obsidian Notes vault."
  :group 'obsidian
  :type 'directory)

(eval-when-compile (defvar local-minor-modes))

(defvar obsidian--tags-list nil "List of Obsidian Notes tags generated by obsidian.el.")
(defvar obsidian--tag-regex "#[[:alnum:]-_=+]+" "Regex pattern used to find tags in Obsidian files.")

(defun obsidian-specify-path (&optional path)
  "Specifies obsidian folder PATH to obsidian-folder variable.

When run interactively asks user to specify the path."
  (interactive)
  (->> (or path (read-directory-name "Specify path to Obsidian folder"))
       (expand-file-name)
       (customize-set-value 'obsidian-directory)))

(-comment
 (obsidian-specify-path)
 "Use the below vault for testing and development"
 (obsidian-specify-path "./tests/test_vault"))

;;; File utilities
;; Copied from org-roam's org-roam-descendant-of-p
(defun obsidian-descendant-of-p (a b)
  "Return t if A is descendant of B."
  (unless (equal (file-truename a) (file-truename b))
    (string-prefix-p (replace-regexp-in-string "^\\([A-Za-z]\\):" 'downcase (expand-file-name b) t t)
		     (replace-regexp-in-string "^\\([A-Za-z]\\):" 'downcase (expand-file-name a) t t))))

(defun obsidian-not-trash-p (file)
  "Return t if FILE is not in .trash of Obsidian."
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
- Pass the `obsidian-file?' check"
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
  "Return string contents of a file or current buffer.

If FILE is not specified, use the current buffer."
  (if file
      (with-temp-buffer
	(insert-file-contents file)
	(buffer-substring-no-properties (point-min) (point-max)))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun obsidian-find-tags (s)
  "Finda all #tags in string.
Argument S string to find tags in."
  (->> (s-match-strings-all obsidian--tag-regex s)
       -flatten))

(defun obsidian-tag? (s)
  "Return t if S will match `obsidian--tag-regex', else nil."
  (when (s-match obsidian--tag-regex s)
    t))

(-comment
 (obsidian-tag? "#foo"))

(defun obsidian-find-tags-in-file (&optional file)
  "Return all tags in file or current buffer.

If FILE is not specified, use the current buffer"
  (-> (obsidian-read-file-or-buffer file)
      obsidian-find-tags
      -distinct))

(defun obsidian-list-all-tags ()
  "Find all tags in all obsidian files."
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
  "Scans entire Obsidian vault and update all tags for completion."
  (->> (obsidian-list-all-tags)
       (setq obsidian--tags-list))
  (message "Obsidian tags updated"))

(-comment
 (obsidian-update-tags-list))

(define-minor-mode obsidian-mode
  "Toggle minor `obsidian-mode' on and off.

Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state."
  ;; The initial value.
  :init-value nil
  :lighter "obs"
  :after-hook (obsidian-update-tags-list))

(defun obsidian-prepare-tags-list (tags)
  "Prepare a list of TAGS with both lower-case and capitalized versions.

Obsidian Notes doesn't considers tags to be the same no matter their case.
Sometimes it's convenient to capitalize a tag, for example when using it
at the start of the sentence.  This function allows completion with both
lower and upper case versions of the tags."
  (let* ((lower-case (->> tags
			  (-map (lambda (s) (s-replace "#" "" s)))
			  (-map 's-downcase)))
	 (capitalized (-map 's-capitalize lower-case))
	 (merged (-concat lower-case capitalized)))
    (->> merged
	 (-map (lambda (s) (s-concat "#" s)))
	 -distinct)))

(-comment
 (->> (obsidian-list-all-tags)
      (obsidian-prepare-tags-list)))

(defun obsidian-tags-backend (command &optional arg &rest ignored)
  "Completion backend for company used by obsidian.el.
Argument COMMAND company command.
Optional argument ARG word to complete.
Optional argument IGNORED this is ignored."
  (interactive (list 'interactive))

  (cl-case command
    (interactive (company-begin-backend 'obsidian-tags-backend))
    (prefix (when (and
		   (-contains? local-minor-modes 'obsidian-mode)
		   (looking-back obsidian--tag-regex nil))
	      (match-string 0)))
    (candidates (->> obsidian--tags-list
		     obsidian-prepare-tags-list
		     (-filter (lambda (s) (s-starts-with? arg s)))))))

(defun obsidian-enable-minor-mode ()
  "Check if current buffer is an `obsidian-file?' and enable minor `obsidian-mode'."
  (when (obsidian-file?)
    (obsidian-mode t)))

(defun obsidian-update ()
  "Command update everything there is to update in obsidian.el (tags, links etc.)."
  (interactive)
  (obsidian-update-tags-list)
  (message "obsidian.el updated"))

(add-hook 'markdown-mode-hook #'obsidian-enable-minor-mode)
(add-to-list 'company-backends 'obsidian-tags-backend)

(provide 'obsidian)

(provide 'obsidian)

;;; obsidian.el ends here
