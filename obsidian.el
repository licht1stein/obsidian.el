;;; obsidian.el --- Obsidian Notes interface -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (c) 2022 Mykhaylo Bilyanskyy <mb@blaster.ai>

;; Author: Mykhaylo Bilyanskyy
;; URL: https://github.com/licht1stein/obsidian.el
;; Keywords: obsidian, pkm, convenience
;; Version: 1.4.4
;; Package-Requires: ((emacs "27.2") (f "0.2.0") (s "1.12.0") (dash "2.13") (markdown-mode "2.5") (elgrep "1.0.0") (yaml "0.5.1"))
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
;; Obsidian.el lets you interact with more convenience with markdown files
;; that are contained in Obsidian Notes vault.  It adds autocompletion for
;; tags and links, jumping between notes, capturing new notes into inbox etc.
;;
;; This allows you to use Emacs for editing your notes, leaving the Obsidian
;; app for syncing and doing more specialized stuff, like viewing notes graphs.

;;; Code:

(require 'f)
(require 'dash)
(require 's)

(require 'cl-lib)

(require 'markdown-mode)

(require 'elgrep)
(require 'yaml)

;; Inspired by RamdaJS's tap function
(defun obsidian-tap (a f)
  "Evaluate (F A) for its side-effects but return A."
  (funcall f a)
  a)

;; Clojure style comment
(defmacro obsidian-comment (&rest _)
  "Ignore body, yield nil."
  nil)

(defgroup obsidian nil "Obsidian Notes group." :group 'text)

(defcustom obsidian-directory nil
  "Path to Obsidian Notes vault."
  :type 'directory)

(defcustom obsidian-inbox-directory nil
  "Subdir to create notes using `obsidian-capture'."
  :type 'directory)

(defcustom obsidian-links-use-vault-path nil
  "If true, the full vault path for a link will be used instead of just the filename."
  :type 'boolean)

(defcustom obsidian-include-hidden-files t
  "If true, files beginning with a period are considered valid Obsidian files."
  :type 'boolean)

;;;###autoload
(defcustom obsidian-wiki-link-create-file-in-inbox t
  "Controls where to create a new file from a wiki link if its target is missing.
    If it is true, create in inbox, otherwise next to the current buffer."
  :type 'boolean)

(defcustom obsidian-daily-notes-directory obsidian-inbox-directory
  "Subdir to create daily notes with `obsidian-daily-note'. Default: the inbox directory"
  :type 'directory)

(defcustom obsidian-templates-directory nil
  "Subdir containing templates"
  :type 'directory)

(defcustom obsidian-daily-note-template "Daily Note Template.md"
  "Daily notes' template filename in templates directory"
  :type 'file)

(defcustom obsidian--use-update-timer nil
  "Determines whether a polling cache update will be used.
If it is true, a timer will be created using the values of
'obsidian-cache-expiry' and 'obsidian-update-idle-wait'."
  :type 'boolean)

(defcustom obsidian-cache-expiry (* 60 5)
  "The number of seconds before the Obsidian cache updates"
  :type 'integer
  :group 'obsidian)

(defcustom obsidian-update-idle-wait 5
  "The number of seconds after cache expiry to wait for Emacs to be idle before running update function."
  :type 'integer
  :group 'obsidian)

(eval-when-compile (defvar local-minor-modes))

(defun obsidian--directory-files-pre28
  (orig-func dir &optional full match nosort ignored)
  "Version of `directory-files' compatible with Emacs versions < 28.

ORIG-FUNC is the original `directory-files' function that is going to be
advised,and DIR and the directory of files on which `directory-files' will
be called.
FULL, MATCH, and NOSORT are the optional arguments for the `directory-files'
function, while IGNORED is the optional 4th argument used with newer versions
of `dirctory-files'."
  (apply orig-func dir full match nosort))

(if (< emacs-major-version 28)
    (advice-add 'directory-files :around #'obsidian--directory-files-pre28))

;;;###autoload
(defun obsidian-specify-path (&optional path)
  "Specifies obsidian folder PATH to obsidian-folder variable.

When run interactively asks user to specify the path."
  (interactive)
  (->> (or path (read-directory-name "Specify path to Obsidian folder"))
       (expand-file-name)
       (customize-set-value 'obsidian-directory)))

(define-minor-mode obsidian-mode
  "Toggle minor `obsidian-mode' on and off.

Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state."
  ;; The initial value.
  :init-value nil
  :lighter " obs"
  ;; :after-hook (obsidian-update)
  :keymap (make-sparse-keymap))

(defvar obsidian--tag-regex "#[[:alnum:]-_/+]+" "Regex pattern used to find tags in Obsidian files.")

(defvar obsidian--basic-wikilink-regex "\\[\\[[[:graph:][:blank:]]*\\]\\]"
  "Regex pattern used to find wikilinks.")

(defvar obsidian--basic-markdown-link-regex "\\[[[:graph:][:blank:]]+\\]\([[:graph:][:blank:]]*\)"
  "Regex pattern used to find markdown links.")

(defvar obsidian--files-hash-cache nil
  "Cache for Obsidian files.
{<filepath>: {'tags: <list-of-tags>
              'aliases: <list-of-aliases>}}")

(defvar obsidian--aliases-map (make-hash-table :test 'equal) "Alist of all Obsidian aliases.")

(defvar obsidian--update-timer nil "Timer to periodically update the cache.")
;; TODO: We can use this to check to see if any files are newer than this
;;       and may therefore need to be updated
(defvar obsidian--updated-time nil "Timer when the last update occurred.")

(defun obsidian--stringify (obj)
  "Return OBJ as a string regardless of input type."
  (format "%s" obj))

(defun obsidian--set-tags (file tag-list)
  "Set list TAG-LIST to FILE in files cache."
  (when tag-list
    (if-let ((attr-map (gethash file obsidian--files-hash-cache)))
        (puthash 'tags tag-list attr-map)
      (message "Unable to add tags for %s:\nAvailable keys:\n%s"
               file (s-join "\n" (hash-table-keys obsidian--files-hash-cache))))))

(defun obsidian--set-aliases (file alias-list)
  "Set list ALIAS-LIST to FILE in files cache."
  ;; Update alias hashtable
  (when alias-list
    (if-let ((maliases (obsidian--mapped-aliases file)))
        (let ((new-aliases (-difference alias-list maliases))
               (stale-aliases (-difference maliases alias-list)))
          (when new-aliases
            (seq-map (lambda (alias) (obsidian--add-alias alias file)) new-aliases))
          (when stale-aliases
            (seq-map (lambda (alias) (obsidian--remove-alias alias)) stale-aliases)))
      (seq-map (lambda (alias) (obsidian--add-alias alias file)) alias-list))
    (when-let ((attr-map (gethash file obsidian--files-hash-cache)))
      (puthash 'aliases alias-list attr-map))))

(defun obsidian--set-links (file links-map)
  "Set table LINKS-MAP to FILE in files cache."
  (when links-map
    (if-let ((attr-map (gethash file obsidian--files-hash-cache)))
        (puthash 'links links-map attr-map)
      (message "Unable to add links for %s:\nAvailable keys:\n%s"
               file (s-join "\n" (hash-table-keys obsidian--files-hash-cache))))))

(defun obsidian--add-alias (alias file)
  "Add ALIAS as key to `obsidian--aliases-map' with FILE as value."
  (puthash alias file obsidian--aliases-map))

(defun obsidian--remove-alias (alias)
  "Remove ALIAS as key to `obsidian--aliases-map'."
  (remhash alias obsidian--aliases-map))

(defun obsidian--get-alias (alias &optional dflt)
  "Find ALIAS in `obsidian--aliases-map' with optional DFLT."
  (gethash alias obsidian--aliases-map dflt))

(defun obsidian-aliases ()
  "Return all existing aliases (without values)."
  (hash-table-keys obsidian--aliases-map))

(defun obsidian--user-directory-p (&optional file)
  "Return t if FILE is a user defined directory inside `obsidian-directory'."
  (and (file-directory-p file)
       (obsidian--not-dot-obsidian-p file)
       (obsidian--not-trash-p file)))

(defun obsidian--dot-file-p (p)
  "Return t if path P points to a dot file."
  (s-starts-with-p "." (file-name-base p)))

(defun obsidian--not-trash-p (file)
  "Return t if FILE is not in .trash of Obsidian."
  (not (s-contains-p "/.trash" file)))

(defun obsidian--not-dot-obsidian-p (file)
  "Return t if FILE is not in .obsidian dir of Obsidian."
  (not (s-contains-p "/.obsidian" file)))

(defun obsidian--file-p (&optional file)
  "Return t if FILE is an obsidian.el file, nil otherwise.

If FILE is not specified, use the current buffer's file-path.
FILE is an Org-roam file if:
- It's located somewhere under `obsidian-directory
- It is a markdown .md file
- Is not a dot file or, if `obsidian-include-hidden-files' is t, then:
  - It is not in .trash
  - It is not an Emacs temp file"

  (-when-let* (;; (_ (print (format "NOT True for %s" file)))
               (path (or file (buffer-file-name (buffer-base-buffer))))
               ;; TODO: This won't work if hidden files is true
               (md-ext (s-ends-with-p ".md" path))
               (not-dot-file (or obsidian-include-hidden-files
                                 (not (obsidian--dot-file-p path))))
               (_ (not (string-match-p (rx (or "node_modules" ".git")) path)))
               (not-trash-p (obsidian--not-trash-p path))
               (not-dot-obsidian (obsidian--not-dot-obsidian-p path))
               ;; (relative-path (file-relative-name path obsidian-directory))
               ;; (not-temp-p (not (s-contains-p "~" relative-path)))

               ;; I think (untested) that s-ends-with-p is much faster than s-contians-p
               ;; But this should also be unnecessary because of the md-ext line
               ;; (not-temp-p (not (s-ends-with-p "~" path)))
               )
    t))

(defun obsidian--file-relative-name (f)
  "Take file name F and return relative path for `obsidian-directory'."
  (file-relative-name f obsidian-directory))

(defun obsidian--expand-file-name (f)
  "Take relative file name F and return expanded name."
  (expand-file-name f obsidian-directory))

(defun obsidian-files ()
  "Lists all Obsidian Notes files that are not in trash."
  (when obsidian--files-hash-cache
    (hash-table-keys obsidian--files-hash-cache)))

(defun obsidian-cached-file-p (file)
  "Retrun true if FILE exists in files cache."
  (seq-contains-p (obsidian-files) file))

(defun obsidian-clear-cache ()
  "Clears the obsidian.el cache.

If you need to run this manually, please report this as an issue on Github."
  (interactive)
  (setq obsidian--files-hash-cache nil))

(defun obsidian-directories ()
  "Lists all Obsidian sub folders."
  (->> (directory-files-recursively obsidian-directory "" t)
       (-filter #'obsidian--user-directory-p)))

(defun obsidian--read-file-or-buffer (&optional file)
  "Return string contents of a file or current buffer.

If FILE is not specified, use the current buffer."
  (if (and file (file-exists-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-substring-no-properties (point-min) (point-max)))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun obsidian--find-tags-in-string (s)
  "Retrieve list of #tags from buffer section string S."
  (let ((front-matter (obsidian--find-yaml-front-matter s))
        (add-tag-fn (lambda (tag) (concat "#" tag))))
    (->> (s-match-strings-all obsidian--tag-regex s)
         (append (and front-matter (mapcar add-tag-fn (gethash 'tags front-matter))))
         -flatten)))

(defun obsidian--find-aliases-in-string (s)
  "Retrieve list of aliases from buffer section string S."
  (when-let ((dict (obsidian--find-yaml-front-matter s)))
    (let* ((aliases-val (gethash 'aliases dict))
           ;; yaml parser can return a value of :null
           (aliases (when (not (equal :null aliases-val)) aliases-val))
           (alias (gethash 'alias dict))
           (all-aliases (append aliases (list alias))))
      (seq-map #'obsidian--stringify (-distinct (-filter #'identity all-aliases))))))

;; TODO: Could we use something like markdown-next-link ?
;; TODO: Can I do this without an additional call to with-temp-buffer ?
(defun obsidian--find-links-in-string (s)
  "Retrieve hashtable of links from buffer section string S.

Values of hashtabale are lists with values that matche those returned by
markdown-link-at-pos:
  0. beginning position
  1. end position
  2. link text
  3. URL
  4. reference label
  5. title text
  6. bang (nil or \"!\")"
  ;; (save-excursion
  (with-temp-buffer
    (insert s)
    (let ((dict (make-hash-table)))
      (goto-char (point-min))
      (while (markdown-match-generic-links (point-max) nil)
        (let ((link-info (markdown-link-at-pos (point))))
          ;; TODO: Using a hashmap means we can only have a single link to
          ;;       a file within a different file. Is that okay?
          (puthash (nth 3 link-info) link-info dict)))
      ;; (message (format "Found %d links" (length (hash-table-keys dict))))
      dict)))

(defun obsidian--find-yaml-front-matter (s)
  "Find YAML front matter in string section S."
  (if (s-starts-with-p "---" s)
      (let* ((split (s-split-up-to "---" s 2))
             (looks-like-yaml-p (eq (length split) 3)))
        (if looks-like-yaml-p
            (->> split
                 (nth 1)
                 yaml-parse-string)))))

(defun obsidian-tags ()
  "List of Obsidian Notes tags generated by obsidian.el."
  (when obsidian--files-hash-cache
    (-distinct
     (remove nil
             (-mapcat (lambda (val-map)
                        (gethash 'tags val-map))
                      (hash-table-values obsidian--files-hash-cache))))))

(defun obsidian-file-metadata (&optional file)
  "Find the tags, aliases, and links in FILE and return as hashtable."
  ;; (message "Processing file %s" file)
  (-let* ((bufstr (obsidian--read-file-or-buffer file))
          (filename (or file (buffer-file-name)))
          (tags (obsidian--find-tags-in-string bufstr))
          (aliases (obsidian--find-aliases-in-string bufstr))
          (links (obsidian--find-links-in-string bufstr))
          (meta (make-hash-table :size 3)))
    (puthash 'tags tags meta)
    (puthash 'aliases aliases meta)
    (puthash 'links links meta)
    meta))

(defun obsidian--update-file-metadata (&optional file)
  "Update the metadata for the file FILE.

If file is not specified, the current buffer will be used."
  (-let* ((filename (or file (buffer-file-name)))
          (meta (obsidian-file-metadata filename)))
    (obsidian--set-tags filename (gethash 'tags meta))
    (obsidian--set-aliases filename (gethash 'aliases meta))
    (obsidian--set-links filename (gethash 'links meta))))

(defun obsidian-prepare-tags-list (tags)
  "Prepare a list of TAGS with both lower-case and capitalized versions.

Obsidian Notes tags are case-independent and are therefore considered to be
the same no matter their case.  Sometimes it's convenient to capitalize a
tag, for example when using it at the start of the sentence.  This function
allows completion with both lower and upper case versions of the tags."
  (let* ((lower-case (->> tags
                          (-map (lambda (s) (s-replace "#" "" s)))
                          (-map #'s-downcase)))
         (capitalized (-map #'s-capitalize lower-case))
         (merged (-concat tags lower-case capitalized)))
    (->> merged
         (-map (lambda (s) (s-concat "#" s)))
         -distinct)))

(defun obsidian-tags-backend (command &rest arg)
  "Completion backend for company used by obsidian.el.
Argument COMMAND company command.
Optional argument ARG word to complete."
  (interactive (if (and (featurep 'company)
                        (fboundp 'company-begin-backend))
                   (company-begin-backend 'obsidian-tags-backend)
                 (error "Company not installed")))
  (cl-case command
    (prefix (when (and
                   (-contains-p local-minor-modes 'obsidian-mode)
                   (looking-back obsidian--tag-regex nil))
              (match-string 0)))
    (candidates (->> (obsidian-prepare-tags-list (obsidian-tags))
                     (-filter (lambda (s) (s-starts-with-p (car arg) s)))))))

(defun obsidian-enable-minor-mode ()
  "Check if current buffer is an `obsidian--file-p' and toggle `obsidian-mode'."
  (and (derived-mode-p 'markdown-mode)
       (obsidian--file-p)
       (obsidian-mode t)))

(defun obsidian-update ()
  "Command update everything there is to update in obsidian.el (tags, links etc.)."
  (interactive)
  ;; TODO: Once the cache is initially populated, we'd like a better way to update
  ;;       than a full recreation
  (-let* ((all-files (directory-files-recursively obsidian-directory "\.*$"))
          (obs-files (-filter #'obsidian--file-p all-files))
          (file-count (length obs-files)))
    (setq obsidian--files-hash-cache (make-hash-table :test 'equal :size file-count))
    (-map #'obsidian--add-file obs-files)
    (message "Obsidian cache updated at %s (%d files)"
             (format-time-string "%H:%M:%S") file-count)
    file-count))

(defun obsidian-update-async ()
  "Asyncrhonous version of obsidian-update."
  (interactive)
  (async-start
   (lambda () (function obsidian-update))
   (lambda (_) (message "Obsidian cache asynchronously reset"))))

(defun obsidian--format-link (file-path &optional toggle)
  "Format link from FILE-PATH based on `obsidian-links-use-vault-path'.

Will format FILE-PATH based on `obsidian-links-use-vault-path' and an optional
prefix argument TOGGLE. If link contains a colon (:), it is assumed to not be an
Obsidian link and is returned unmodified."
  (if (s-contains-p ":" file-path)
      file-path
    (if obsidian-links-use-vault-path
        (if toggle (file-name-nondirectory file-path) file-path)
      (if toggle file-path (file-name-nondirectory file-path)))))

(defun obsidian--verify-relative-path (f)
  "Check that file F exists, and create it if it does not. F will be a relative path."
  (if (s-contains-p ":" f)
      f
    (let* ((obs-path (obsidian--expand-file-name f))
           (exists (obsidian-cached-file-p obs-path)))
      (if (not exists)
          (obsidian--file-relative-name (obsidian--prepare-new-file-from-rel-path f))
        f))))

(defun obsidian--request-link (&optional toggle-path)
  "Service function to request user for link input.

TOGGLE-PATH is a boolean that will toggle the behavior of
`obsidian-links-use-vault-path' for this single link insertion."
  (let* ((all-files (->> (obsidian-files) (-map (lambda (f) (file-relative-name f obsidian-directory)))))
         (region (when (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))))
         (chosen-file (completing-read "Link: " all-files))
         (verified-file (obsidian--verify-relative-path chosen-file))
         (default-description (-> verified-file file-name-nondirectory file-name-sans-extension))
         (description (read-from-minibuffer "Description (optional): " (or region default-description)))
         (file-link (obsidian--format-link verified-file toggle-path)))
    (list :file file-link :description description)))

;;;###autoload
(defun obsidian-insert-wikilink (&optional arg)
  "Insert a link to file in wikilink format.

If ARG is set, the value of `obsidian-links-use-vault-path' will be toggled for
the current link insertion."
  (interactive "P")
  (let* ((file (obsidian--request-link arg))
         (filename (plist-get file :file))
         (description (plist-get file :description))
         (no-ext (file-name-sans-extension filename))
         (link (if (and description (not (s-ends-with-p description no-ext)))
                   (s-concat "[[" no-ext "|" description"]]")
                 (s-concat "[[" no-ext "]]"))))
    (insert link)))

;;;###autoload
(defun obsidian-insert-link (&optional arg)
  "Insert a link to file in markdown format.

If ARG is set, the value of `obsidian-links-use-vault-path' will be toggled for
this link insertion. If text is highlighted, the highlighted text will be
replaced by the link."
  (interactive "P")
  (let* ((file-plist (obsidian--request-link arg))
         (file-raw (plist-get file-plist :file))
         (file (s-replace " " "%20" file-raw))
         (description (plist-get file-plist :description))
         (link-str (s-concat "[" description "](" file ")")))
    (if (use-region-p)
        (delete-active-region))
    (insert link-str)))

;;;###autoload
(defun obsidian-capture ()
  "Create new obsidian note.

In the `obsidian-inbox-directory' if set otherwise in `obsidian-directory' root."
  (interactive)
  (let* ((title (read-from-minibuffer "Title: "))
         (filename (s-concat obsidian-directory "/" obsidian-inbox-directory "/" title ".md"))
         (clean-filename (s-replace "//" "/" filename)))
    (find-file (expand-file-name clean-filename) t)
    (save-buffer)))

;;;###autoload
(defun obsidian-daily-note ()
  "Create new obsidian daily note.

In the `obsidian-daily-notes-directory' if set otherwise in `obsidian-inbox-directory' - if that's also unset,
in `obsidian-directory' root.
."
  (interactive)
  (let* ((title (format-time-string "%Y-%m-%d"))
         (filename (s-concat obsidian-directory "/" obsidian-daily-notes-directory "/" title ".md"))
         (clean-filename (s-replace "//" "/" filename)))
    (find-file (expand-file-name clean-filename) t)
    (save-buffer)
    (when (and obsidian-templates-directory
               obsidian-daily-note-template
               (eq (buffer-size) 0))
      (obsidian-apply-template
       (s-concat obsidian-directory "/"
                 obsidian-templates-directory "/"
                 obsidian-daily-note-template))
      (save-buffer))))

;; TODO: allow links to subsections of a note
;;;###autoload
(defun obsidian-jump ()
  "Jump to Obsidian note."
  (interactive)
  ;; (obsidian-update)
  (let* ((files (obsidian-files))
         (dict (make-hash-table :test 'equal))
         (_ (-map (lambda (f)
                    (puthash (file-relative-name f obsidian-directory) f dict))
                  files))
         (choices (-sort #'string< (-distinct (-concat (obsidian-aliases) (hash-table-keys dict)))))
         (choice (completing-read "Jump to: " choices))
         (target (obsidian--get-alias choice (gethash choice dict))))
    (if target
        (find-file target)
      (user-error "Note not found: %s" choice))))

(defun obsidian--mapped-aliases (file)
  "Return list of aliases mapped to FILE in obsidian--aliases-map."
  (let ((aliases '()))
    (maphash (lambda (k v)
               (when (equal file v)
                 (add-to-list 'aliases k)))
             obsidian--aliases-map )
    aliases))

;; (defun obsidian--upsert-file (file)
(defun obsidian--add-file (file)
  "Add a FILE to the files cache and update tags and aliases for the file."
  (when (not (gethash file obsidian--files-hash-cache))
    (puthash file (make-hash-table :size 3) obsidian--files-hash-cache))
  (obsidian--update-file-metadata file))

(defun obsidian--remove-file (file)
  "Remove FILE from the files cache and update tags and aliases accordingly."
  (-map #'obsidian--remove-alias (obsidian--mapped-aliases file))
  (remhash file obsidian--files-hash-cache))

;; TODO: Is there a hook for file remove?
;; TODO: after-change-functions hook(s) ?
(defun obsidian--update-on-save ()
  (when (obsidian--file-p (buffer-file-name))
    (obsidian--add-file (buffer-file-name))))

;;;###autoload
(defun obsidian-move-file ()
  "Move current note to another directory."
  (interactive)
  (when (not (obsidian--file-p (buffer-file-name)))
    (user-error "Current file is not an obsidian-file"))
  (let* ((old-file-path (buffer-file-name))
         (dict (make-hash-table :test 'equal))
         (_ (-map (lambda (d)
                    (puthash (file-relative-name d obsidian-directory) d dict))
                  (obsidian-directories)))
         (choice (completing-read "Move to: " (hash-table-keys dict)))
         (new-file-directory (file-name-as-directory (gethash choice dict)))
         (new-file-path (expand-file-name (file-name-nondirectory old-file-path) new-file-directory)))
    (when (equal new-file-path old-file-path)
      (user-error "File already exists at that location"))
    (rename-file old-file-path new-file-directory)
    (write-file new-file-path)
    (obsidian--remove-file old-file-path)
    (message "Moved to %s" new-file-path)))

(defun obsidian-prepare-file-path (s)
  "Replace %20 with spaces in file path.
Argument S relative file name to clean and convert to absolute."
  (let* ((cleaned-name (s-replace "%20" " " s)))
    cleaned-name))

(defun obsidian--match-files (f all-files)
  "Filter ALL-FILES to return list with same name as F."
  (-filter (lambda (el) (or (s-equals-p f el) (s-ends-with-p (concat "/" f) el))) all-files))

(defun obsidian--prepare-new-file-from-rel-path (p)
  "Create file if it doesn't exist and return full system path for relative path P.

If the file include directories in its path, we create the file relative to
`obsidian-directory'. If there are no paths, we create the new file in
`obsidian-inbox-directory' if set, otherwise in `obsidian-directory'."
  (let* ((f (if (not (file-name-extension p)) (s-concat p ".md") p))
         (filename (if (s-contains-p "/" f)
                       (s-concat obsidian-directory "/" f)
                     (s-concat obsidian-directory "/"
                               obsidian-inbox-directory "/" f)))
         (cleaned (s-replace "//" "/" filename)))
    (when (not (f-exists-p cleaned))
      (f-mkdir-full-path (f-dirname cleaned))
      (f-touch cleaned)
      (obsidian--add-file cleaned))
    cleaned))

(defun obsidian-find-file (f &optional arg)
  "Take file F and either opens directly or offer choice if multiple match.

If ARG is set, the file will be opened in other window."
  (let* ((all-files (->> (obsidian-files) (-map #'obsidian--file-relative-name)))
         (matches (obsidian--match-files f all-files))
         (file (cl-case (length matches)
                 (0 (obsidian--prepare-new-file-from-rel-path (obsidian--maybe-in-same-dir f)))
                 (1 (car matches))
                 (t
                  (let* ((choice (completing-read "Jump to: " matches)))
                    choice))))
         (find-fn (if arg #'find-file-other-window #'find-file)))
    (funcall find-fn (obsidian--expand-file-name file))))

(defun obsidian--maybe-in-same-dir (f)
  "If `f` contains '/', returns f, otherwise with buffer, relative to the buffer"
  (if (s-contains-p "/" f)
      f
    (if obsidian-wiki-link-create-file-in-inbox
        f
      (concat (file-relative-name (file-name-directory (buffer-file-name)) obsidian-directory) "/" f))))

(defun obsidian--wiki-link-p ()
  "Return non-nil if `point' is at a true wiki link.
A true wiki link name matches `markdown-regex-wiki-link' but does
not match the current file name after conversion.  This modifies
the data returned by `match-data'.  Note that the potential wiki
link name must be available via `match-string'."
  (let ((case-fold-search nil))
    (and (thing-at-point-looking-at markdown-regex-wiki-link)
         (not (markdown-code-block-at-point-p))
         (or (not buffer-file-name)
             (not (string-equal (buffer-file-name)
                                (markdown-wiki-link-link)))))))

(defsubst obsidian--remove-section (s)
  "Remove section from file path.
   From 'filename#section' keep only the 'filename'."
  (replace-regexp-in-string "#.*$" "" s))

(defun obsidian-wiki->normal (f)
  "Add extension to wiki link F if none."
  (if (file-name-extension f)
      f
    (s-concat (obsidian--remove-section f) ".md")))

(defun obsidian-follow-wiki-link-at-point (&optional arg)
  "Find Wiki Link at point. Opens wiki links in other window if ARG is non-nil."
  (interactive "P")
  ;; (obsidian--wiki-link-p)
  (thing-at-point-looking-at markdown-regex-wiki-link)
  (let* ((url (->> (match-string-no-properties 3)
                   s-trim)))
    (if (s-contains-p ":" url)
        (browse-url url)
      (-> url
          obsidian-prepare-file-path
          obsidian-wiki->normal
          (obsidian-tap #'message)
          (obsidian-find-file arg)))))

(defun obsidian-follow-markdown-link-at-point (&optional arg)
  "Find and follow markdown link at point.
Opens markdown links in other window if ARG is non-nil.."
  (interactive "P")
  (let ((normalized (s-replace "%20" " " (markdown-link-url))))
    (if (s-contains-p ":" normalized)
        (browse-url normalized)
      (-> normalized
          obsidian-prepare-file-path
          (obsidian-find-file arg)))))

;;;###autoload
(defun obsidian-follow-link-at-point (&optional arg)
  "Follow thing at point if possible, such as a reference link or wiki link.
Opens inline and reference links in a browser.  Opens wiki links
to other files in the current window, or another window if
ARG is non-nil.
See `markdown-follow-link-at-point' and
`markdown-follow-wiki-link-at-point'."
  (interactive "P")
  (cond ((markdown-link-p)
         (obsidian-follow-markdown-link-at-point arg))
        ((obsidian--wiki-link-p)
         (obsidian-follow-wiki-link-at-point arg))))

(defun obsidian--grep (re)
  "Find RE in the Obsidian vault."
  (elgrep obsidian-directory "\.md" re
          :recursive t
          :case-fold-search t
          :exclude-file-re (if obsidian-include-hidden-files "~" "^\\.\\|~")
          :exclude-dir-re ".obsidian"))

(defun obsidian--link-p (s)
  "Check if S matches any of the link regexes."
  (when s
    (or (s-matches-p obsidian--basic-wikilink-regex s)
        (s-matches-p obsidian--basic-markdown-link-regex s))))

(defun obsidian--elgrep-get-context (match)
  "Get :context out of MATCH produced by elgrep."
  (when match
    (let* ((result (->> match
                        -flatten))
           (context (plist-get result :context)))
      context)))

(defun obsidian--mention-link-to-p (filename match)
  "Check if `MATCH' produced by `obsidian--grep' contain a link to `FILENAME'."
  (let* ((result (mapcar (lambda (element)
                           ;; (message "ELEMENT ---> %s" (obsidian--elgrep-get-context element))
                           (if (listp element)
                               (and
                                (obsidian--link-p (obsidian--elgrep-get-context element))
                                (string-match-p (format "\\b%s\\b" filename)
                                                (format "%s" (obsidian--elgrep-get-context element))))))
                         (cdr match))))
    (when (remove nil result) t)))

(defun obsidian--file-links (filename)
  "FILENAME is the name and extension without directories

host - host file; the one that includes the link.  full path filename
targ - target file; file being pointed to by the host link.  name and extension only
meta - metadata hashtable
lmap - hashmap of links from meta
link - link target from links hashmap
info - info list for target link from links hashmap

The files cache has the following structure:
  {filepath: {'tags:    (tag list)
              'aliases: (alias list)
              'links:   {linkname: (link info list)}}}"
  (let ((targ filename)
        (resp (make-hash-table)))
    (maphash
     (lambda (host meta)
       (when-let ((links-map (gethash 'links meta)))
         (maphash
          (lambda (link info)
            (when (equal link targ)
              (puthash host info resp)))
          links-map)))
     (obsidian--files-hash-cache))
    resp))

;; TODO: This function can be replaced once the links are in the cache
(defun obsidian--find-links-to-file (filepath)
  "Find any mention of trimmed FILEPATH in the vault.

filepath: the absolute filename including extension
filename: the name of the file with extension without any directories
file-word: the file name without any extension or directory informatoin"
  (-let* ((filename (file-name-nondirectory filepath))
          (file-word (file-name-sans-extension filename))
          (matches (obsidian--grep file-word)))
    ;; (-filter (lambda (m) (obsidian--mention-link-to-p file-word m)) matches)
    (->> (-filter (lambda (m) (obsidian--mention-link-to-p file-word m)) matches)
         (-map #'car))))

(defun obsidian--completing-read-for-matches (coll)
  "Take a COLL of matches produced by elgrep and make a list for completing read."
  (let* ((dict (make-hash-table :test 'equal))
         (_ (-map (lambda (f) (puthash f (obsidian--expand-file-name f) dict)) coll)))
    dict))

(defun obsidian-apply-template (template-filename)
  "Apply the template for the current buffer.
Template vars: {{title}}, {{date}}, and {{time}}"
  (let* ((title (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
         (date (format-time-string "%Y-%m-%d"))
         (time (format-time-string "%H:%M:%S"))
         (m (point))
         (template-content (with-temp-buffer
                             (insert-file-contents template-filename)
                             (buffer-string)))
         (output-content (replace-regexp-in-string "{{title}}" title template-content))
         (output-content (replace-regexp-in-string "{{date}}" date output-content))
         (output-content (replace-regexp-in-string "{{time}}" time output-content)))
    (goto-char (point-min))
    (insert output-content)
    (message "Template variables replaced and inserted to the buffer")
    (goto-char m)))

;;;###autoload
(defun obsidian-backlink-jump ()
  "Select a backlink to this file and follow it."
  (interactive)
  (let* ((backlinks (obsidian--find-links-to-file (buffer-file-name)))
         (dict (obsidian--completing-read-for-matches backlinks))
         (choices (-sort #'string< (-distinct (hash-table-keys dict)))))
    (if choices
        (let* ((choice (completing-read "Jump to: " choices))
               (target (obsidian--get-alias choice (gethash choice dict))))
          (find-file target))
      (message "No backlinks found."))))

;;;###autoload
(defun obsidian-search ()
  "Search Obsidian vault for input."
  (interactive)
  (let* ((query (-> (read-from-minibuffer "Search query or regex: ")))
         (results (obsidian--grep query)))
    (message (s-concat "Found " (pp-to-string (length results)) " matches"))
    (let* ((choice (completing-read "Select file: " results)))
      (obsidian-find-file choice))))

;;;###autoload
(defun obsidian-tag-find ()
  "Find all notes with a tag."
  (interactive)
  (let* ((tag (completing-read "Select tag: "
                               (->> (obsidian-tags) (-map 's-downcase) -distinct (-sort 'string-lessp))))
         (results (obsidian--grep tag))
         (choice (completing-read "Select file: " results)))
    (obsidian-find-file choice)))

(when (eval-when-compile (require 'hydra nil t))
  (defhydra obsidian-hydra (:hint nil)
    "
Obsidian
_f_ollow at point   insert _w_ikilink          _q_uit
_j_ump to note      insert _l_ink              capture daily _n_ote
_t_ag find          _c_apture new note
_s_earch by expr.   _u_pdate tags/alises etc.
"
    ("c" obsidian-capture)
    ("n" obsidian-daily-note)
    ("f" obsidian-follow-link-at-point)
    ("j" obsidian-jump)
    ("l" obsidian-insert-link :color blue)
    ("q" nil :color blue)
    ("s" obsidian-search)
    ("t" obsidian-tag-find)
    ("u" obsidian-update)
    ("w" obsidian-insert-wikilink :color blue)))

;;;###autoload
(define-globalized-minor-mode global-obsidian-mode obsidian-mode obsidian-enable-minor-mode)

(when (boundp 'company-backends)
  (add-to-list 'company-backends 'obsidian-tags-backend))

(add-hook 'after-save-hook 'obsidian--update-on-save)

;; TODO: alternatives to polling:
;;       - how does lsp track files?
;;       - what triggers treemacs to update?
;;         - search treemacs code for "add-hook-"
(defun obsidian-idle-timer ()
  "Wait until Emacs is idle to call update."
  (message "Update timer buzz at %s" (format-time-string "%H:%M:%S"))
  (run-with-idle-timer obsidian-update-idle-wait nil 'obsidian-update))

(when obsidian--use-update-timer
    (setq obsidian--update-timer
          (run-with-timer 0 obsidian-cache-expiry 'obsidian-idle-timer)))

(defun obsidian-stop-update-timer ()
  "Stop the background process that periodically updates the cache."
  (interactive)
  (cancel-timer update-timer))

(provide 'obsidian)
;;; obsidian.el ends here
