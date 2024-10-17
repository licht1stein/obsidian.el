;;; obsidian.el --- Obsidian Notes interface -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (c) 2022 Mykhaylo Bilyanskyy <mb@blaster.ai>

;; Author: Mykhaylo Bilyanskyy
;; URL: https://github.com/licht1stein/obsidian.el
;; Keywords: obsidian, pkm, convenience
;; Version: 1.4.4
;; Package-Requires: ((emacs "27.2") (f "0.2.0") (s "1.12.0") (dash "2.13") (markdown-mode "2.5") (elgrep "1.0.0") (yaml "0.5.1") (ht "2.3"))
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
;; that are contained in an Obsidian Notes vault.  It adds autocompletion for
;; tags and links, jumping between notes, capturing new notes into inbox etc.
;;
;; This allows you to use Emacs for editing your notes, leaving the Obsidian
;; app for syncing and doing more specialized stuff, like viewing notes graphs.

;;; Code:

(require 'f)
(require 'dash)
(require 's)
(require 'ht)
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
  "If true, use the full vault path for a link instead of just the filename."
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
  "Subdir to create daily notes with `obsidian-daily-note'.

Default is the inbox directory"
  :type 'directory)

(defcustom obsidian-templates-directory nil
  "Subdirectory containing templates."
  :type 'directory)

(defcustom obsidian-daily-note-template "Daily Note Template.md"
  "Daily notes' template filename in templates directory."
  :type 'file)

(defcustom obsidian-use-update-timer t
  "Determines whether a polling cache update will be used.
If it is true, a timer will be created using the values of
`obsidian-cache-expiry' and `obsidian-update-idle-wait'."
  :type 'boolean)

(defcustom obsidian-cache-expiry (* 60 5)
  "The number of seconds before the Obsidian cache will update."
  :type 'integer
  :group 'obsidian)

(defcustom obsidian-update-idle-wait 5
  "Seconds to wait after cache expiry for Emacs to be idle before running update."
  :type 'integer
  :group 'obsidian)

(eval-when-compile (defvar local-minor-modes))

(defun obsidian--directory-files-pre28
    (orig-func dir &optional full match nosort _)
  "Version of `directory-files' compatible with Emacs versions < 28.

ORIG-FUNC is the original `directory-files' function that is going to be
advised,and DIR and the directory of files on which `directory-files' will
be called.
FULL, MATCH, and NOSORT are the optional arguments for the `directory-files'
function, while _ is the optional 4th argument used with newer versions
of `dirctory-files'."
  (apply orig-func dir full match nosort))

(if (< emacs-major-version 28)
    (advice-add 'directory-files :around #'obsidian--directory-files-pre28))

;;;###autoload
(defun obsidian-specify-path (&optional path)
  "Set `obsidian-directory' to PATH or user-selected directory.

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
  :after-hook (obsidian-update)
  :keymap (make-sparse-keymap))

(defvar obsidian--debug-messages nil "Additional messages will be displayed for debugging.")

(defvar obsidian--tag-regex
  "\\(?:\\`\\|[[:space:]]\\)\\#\\(?:[A-Za-z_/][-A-Za-z0-9_/]*[A-Za-z_/][-A-Za-z0-9_/]*\\)"
  "Regex pattern used to find tags in Obsidian files.

Here's a breakdown of the pattern:

- `\\(?:\\`\\|[[:space:]]\\)`: Matches the beginning of the document (`\\``) or
any whitespace character (`[[:space:]]`). This ensures that the tag either
starts at the beginning of the document or is preceded by a whitespace character.

- `\\(#`: Matches the starting hashtag of the tag and begins a capturing group
for the tag itself.

- `\\(?:[A-Za-z_/][-A-Za-z0-9_/]*[A-Za-z_/][-A-Za-z0-9_/]*\\)`: This ensures that
the tag has at least one non-numerical character (A-Z) and allows the use of
letters,numbers, underscores, hyphens, and forward slashes. The first and last
segments (`[A-Za-z_/]`) ensure that the tag contains at least one non-numeric
character outside the start and end. Numbers will not be allowed as the only
characters of a tag.

- `[#)]`: Ends the capturing group for the whole tag that starts with a `#`.")

(defvar obsidian--basic-wikilink-regex "\\[\\[[[:graph:][:blank:]]*\\]\\]"
  "Regex pattern used to find wikilinks.")

(defvar obsidian--basic-markdown-link-regex "\\[[[:graph:][:blank:]]+\\]\([[:graph:][:blank:]]*\)"
  "Regex pattern used to find markdown links.")

(defvar obsidian--vault-cache nil
  "Cache for Obsidian files.

The cache is a hashmap with the following structure
{<filepath>: {tags: <list-of-tags>
              aliases: <list-of-aliases>}}
              links: <list-of-link-lists>}}

Each link list contains the following as returned by markdown-link-at-pos:
  0. beginning position
  1. end position
  2. link text
  3. URL
  4. reference label
  5. title text
  6. bang (nil or \"!\")")

(defvar obsidian--tags-map "Hash table with tags as keys and list of files as values.")

(defvar obsidian--file-metadata nil "Hash table with file metadata (tags, aliases, links.")

(defvar obsidian--aliases-map (make-hash-table :test 'equal) "Hash table of all Obsidian aliases.")

(defvar obsidian--backlinks-alist (make-hash-table :test 'equal) "Alist of backlinks.")

(defvar obsidian--jump-list nil "List of buffer locations visited via jump.")

(defvar obsidian--update-timer nil "Timer to periodically update the cache.")

(defvar obsidian--updated-time nil
  "Time of last cache update as a float number of seconds since the epoch.")

(defun obsidian--stringify (obj)
  "Return OBJ as a string regardless of input type."
  (format "%s" obj))

(defun obsidian--strip-props (s)
  "Remove all text properties from string S."
  (set-text-properties 0 (length s) nil s)
  s)

(defun obsidian--sort-map-by-values (mm)
  "Return the hashmap MM sorted by the values.

The function is taken from xahlee:
- http://xahlee.info/emacs/emacs/elisp_sort_hash_table.html"
  (let* ((resp (make-hash-table :test 'equal
                                :size (length (hash-table-keys mm))))
         (xlist (let ((yy nil))
                  (maphash
                   (lambda (k v)
                     (push (list k v) yy))
                   mm)
                  yy))
         (xlist (sort xlist (lambda (a b) (string< (car a) (car b)))))
         (xlist (sort xlist (lambda (a b) (< (nth 1 a) (nth 1 b))))))
    (seq-map (lambda (ii) (puthash (nth 0 ii) (nth 1 ii) resp)) xlist)
    resp))

(defun obsidian--set-tags (file tag-list)
  "Set list TAG-LIST to FILE in files cache."
  (when tag-list
    (if-let ((attr-map (gethash file obsidian--vault-cache)))
        (puthash 'tags tag-list attr-map)
      (message "Unable to add tags for %s:\nAvailable keys:\n%s"
               file (s-join "\n" (hash-table-keys obsidian--vault-cache))))))

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
    (when-let ((attr-map (gethash file obsidian--vault-cache)))
      (puthash 'aliases alias-list attr-map))))

(defun obsidian--set-links (file links-map)
  "Set table LINKS-MAP to FILE in files cache."
  (when links-map
    (if-let ((attr-map (gethash file obsidian--vault-cache)))
        (puthash 'links links-map attr-map)
      (message "Unable to add links for %s:\nAvailable keys:\n%s"
               file (s-join "\n" (hash-table-keys obsidian--vault-cache))))))

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
  (-when-let* ((path (or file (buffer-file-name (buffer-base-buffer))))
               (md-ext (s-ends-with-p ".md" path))
               (not-dot-file (or obsidian-include-hidden-files
                                 (not (obsidian--dot-file-p path))))
               (not-node-git-p (not (string-match-p (rx (or "node_modules" ".git")) path)))
               (not-trash-p (obsidian--not-trash-p path))
               (not-dot-obsidian (obsidian--not-dot-obsidian-p path)))
    t))

(defun obsidian--file-relative-name (f)
  "Take file name F and return relative path for `obsidian-directory'."
  (file-relative-name f obsidian-directory))

(defun obsidian--expand-file-name (f)
  "Take relative file name F and return expanded name."
  (expand-file-name f obsidian-directory))

(defun obsidian-files ()
  "Lists all Obsidian Notes files that are not in trash."
  (when obsidian--vault-cache
    (hash-table-keys obsidian--vault-cache)))

(defun obsidian-cached-file-p (file)
  "Retrun true if FILE exists in files cache."
  (seq-contains-p (obsidian-files) file))

(defun obsidian-directories ()
  "Lists all Obsidian sub folders."
  (->> (directory-files-recursively obsidian-directory "" t)
       (-filter #'obsidian--user-directory-p)))

(defun obsidian--process-front-matter-tags (front-matter)
  "FRONT-MATTER is the hashmap from obsidian--find-yaml-front-matter-in-string.

A list of valid tags is returned.

This function filters invalid tags (eg tags that are not in a list, or tags
that already have hashtags as these are not allowed in front matter, or
values of :null as may be returned by the YAML parser), trims whitespace,
and concatenates a hashtag to the beginning of each valid tag."
  (when front-matter
    (let* ((tags (gethash 'tags front-matter)))
      ;; tags in front matter should be specified as a list
      (when (and tags (not (equal 'string (type-of tags))))
        (if (equal tags :null)
            nil
          (seq-map (lambda (tag) (format "#%s" tag))
                   (->> tags
                        ;; spaces are not allowed in tags; use commas between tags
                        (seq-remove (lambda (tag) (s-contains-p " " tag)))
                        ;; tags in front matter shouldn't start with a hashtag
                        (seq-remove (lambda (tag) (s-starts-with-p "#" tag))))))))))

(defun obsidian--find-tags-in-string (s)
  "Retrieve list of #tags from string S.

First searches for front matter to find tags there, then searches through
the entire string."
  (let* ((front-matter (obsidian--find-yaml-front-matter-in-string s))
         (fm-tags (obsidian--process-front-matter-tags front-matter))
         (body-tags-raw (-flatten (s-match-strings-all obsidian--tag-regex s)))
         (body-tags (seq-map #'string-trim-left body-tags-raw)))
    (-flatten (append fm-tags body-tags))))

(defun obsidian--find-aliases-in-string (s)
  "Retrieve list of aliases from string S."
  (when-let ((dict (obsidian--find-yaml-front-matter-in-string s)))
    (let* ((aliases-val (gethash 'aliases dict))
           ;; yaml parser can return a value of :null
           (aliases (when (not (equal :null aliases-val)) aliases-val))
           (alias (gethash 'alias dict))
           (all-aliases (append aliases (list alias))))
      (seq-map #'obsidian--stringify (-distinct (-filter #'identity all-aliases))))))

(defun obsidian--find-links ()
  "Retrieve hashtable of links in current buffer.

Values of hashtabale are lists with values that matche those returned by
markdown-link-at-pos:
  0. beginning position
  1. end position
  2. link text
  3. URL
  4. reference label
  5. title text
  6. bang (nil or \"!\")"
  (let ((dict (make-hash-table :test 'equal)))
    (goto-char (point-min))
    ;; If you search to point-max, you'll get into an infinit loop if there's
    ;; a link a the end of the file, hence (- (point-max 4))
    (while (markdown-match-generic-links (- (point-max) 4) nil)
      (let ((link-info (markdown-link-at-pos (point))))
        (obsidian--strip-props (nth 2 link-info))
        (obsidian--strip-props (nth 3 link-info))
        (puthash (nth 3 link-info) link-info dict)))
    dict))

(defun obsidian--find-yaml-front-matter-in-string (s)
  "Find YAML front matter in string section S."
  (if (s-starts-with-p "---" s)
      (let* ((split (s-split-up-to "---" s 2))
             (looks-like-yaml-p (eq (length split) 3)))
        (if looks-like-yaml-p
            (condition-case nil
                (yaml-parse-string (nth 1 split))
              (error
               (message "Warning: erorr parsing YAML front matter")
               nil))))))

(defun obsidian-tags-ht ()
  "Hashtable with each tags as the keys and list of file path as the values."
  (when obsidian--vault-cache

    (let ((obsidian--tags-map (make-hash-table :test 'equal)))
      ;; loop through files cache to get file/tag list for each file
      (maphash (lambda (file meta)
                 (let ((obsidian--file-metadata meta))
                   ;; loop through the tags list
                   (seq-map (lambda (rawtag)
                              (let ((tag (s-downcase rawtag)))
                                ;; Add the current file to the response list
                                ;; for the current tag in the response hash table
                                (if-let ((file-list (gethash tag obsidian--tags-map)))
                                    (progn
                                      (push (obsidian--file-relative-name file) file-list)
                                      (puthash tag file-list obsidian--tags-map))
                                  (puthash tag (list (obsidian--file-relative-name file))
                                           obsidian--tags-map))))
                            (gethash 'tags obsidian--file-metadata))))
               obsidian--vault-cache)
      (maphash (lambda (k v)
                 (puthash k (-sort 'string-lessp (-distinct v)) obsidian--tags-map))
               obsidian--tags-map)
      obsidian--tags-map)))

(defun obsidian-tags ()
  "List of Obsidian Notes tags generated by obsidian.el."
  (when obsidian--vault-cache
    (-distinct
     (remove nil
             (-mapcat (lambda (val-map)
                        (gethash 'tags val-map))
                      (hash-table-values obsidian--vault-cache))))))

(defun obsidian--buffer-metadata ()
  "Find the tags, aliases, and links in the current buffer and return as hashtable."
  (save-excursion
    (let* ((buf (buffer-substring-no-properties (point-min) (point-max)))
           (tags (obsidian--find-tags-in-string buf))
           (aliases (obsidian--find-aliases-in-string buf))
           (links (obsidian--find-links))
           (meta (make-hash-table :test 'equal :size 3)))
      (puthash 'tags tags meta)
      (puthash 'aliases aliases meta)
      (puthash 'links links meta)
      meta)))

(defun obsidian-file-metadata (&optional file)
  "Find the tags, aliases, and links in FILE and return as hashtable.

Uses current buffer if file is not specified"
  ;; (message "Processing metadata for file %s" file)
  (if (and file (file-exists-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (obsidian--buffer-metadata))
    (obsidian--buffer-metadata)))

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

(defun obsidian-enable-minor-mode ()
  "Check if current buffer is an `obsidian--file-p' and toggle `obsidian-mode'."
  (and (derived-mode-p 'markdown-mode)
       (obsidian--file-p)
       (obsidian-mode t)))

(defun obsidian--find-all-files()
  "Return a list of all obsidian files in the vault."
  (let ((all-files (directory-files-recursively obsidian-directory "\.*$")))
    (-filter #'obsidian--file-p all-files)))

(defun obsidian-clear-cache ()
  "Clears the obsidian.el cache.

If you need to run this manually, please report this as an issue on Github."
  (interactive)
  (setq obsidian--vault-cache nil))

(defun obsidian-populate-cache ()
  "Create an empty cache and populate cache with files, tags, aliases, and links."
  (interactive)
  (-let* ((obs-files (obsidian--find-all-files))
          (file-count (length obs-files)))
    (setq obsidian--vault-cache (make-hash-table :test 'equal :size file-count))
    (message "Populating cache with %d files" file-count)
    (dolist-with-progress-reporter
        (i obs-files)
        (format "Adding Obsidian %d files to cache... " file-count)
      (obsidian--add-file i))
    (message "Obsidian cache populated at %s with %d files"
             (format-time-string "%H:%M:%S") file-count)
    (setq obsidian--updated-time (float-time))
    file-count))

(defun obsidian--updated-externally-p (file)
  "Has FILE been modified by a process other than obsidian.el."
  (let ((file-mod-time (float-time (nth 5 (file-attributes file)))))
    ;; Has the file been modified more recently than obsidian--updated-time
    (> file-mod-time obsidian--updated-time)))

;;;###autoload
(defun obsidian-update ()
  "Check the cache against files on disk and update cache as necessary.

If a file has been modified more recently than `obsidian--updated-time',
we assume it may have been modified outside of obisidian.el so we call
`obisidan--add-file'.  Note that files modified by obsidian.el would also
show more recent modified times if they called `obsidian--update-on-save'
that was triggered by the `after-save-hook'.  We have no way to distinguish
this from a file modified outside of obsidian.el, so we'll re-process
them all just in case."
  (interactive)
  (if (or (not (boundp 'obsidian--vault-cache)) (not obsidian--vault-cache))
      (obsidian-populate-cache)
    (-let* ((cached (obsidian-files))
            (ondisk (obsidian--find-all-files))
            (new-files (-difference ondisk cached))
            (old-files (-difference cached ondisk))
            (to-reprocess (seq-filter #'obsidian--updated-externally-p ondisk)))
      (seq-map #'obsidian--add-file new-files)
      (seq-map #'obsidian--remove-file old-files)
      (seq-map #'obsidian--add-file to-reprocess)
      (setq obsidian--updated-time (float-time))
      (when obsidian--debug-messages
        (when to-reprocess
          (message "Reprocesed the following files:\n%s" (pp to-reprocess)))
        (message "Obsidian cache updated at %s" (format-time-string "%H:%M:%S"))))))

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
  "Check that relative file path F exists, and create it if it does not."
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
                   (if markdown-wiki-link-alias-first
                       (s-concat "[[" description "|" no-ext "]]")
                     (s-concat "[[" no-ext "|" description"]]"))
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
(defun obsidian-remove-link ()
  "Remove link and replace with link text."
  (interactive)
  (cond ((markdown-link-p)
         (let ((link (markdown-link-at-pos (point))))
           (delete-region (nth 0 link) (nth 1 link))
           (insert (or (nth 2 link) (nth 3 link)))))
        ((obsidian-wiki-link-p)
         (markdown-kill-thing-at-point)
         (yank))))

;;;###autoload
(defun obsidian-insert-tag ()
  "Insert a tag from the existing tags."
  (interactive)
  (let* ((tags (-sort #'string< (obsidian-tags)))
         (choice (completing-read "Insert tag: " tags))
         (tag (if (s-starts-with-p "#" choice) choice (format "#%s" choice))))
    (insert tag)))

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

Note is created in the `obsidian-daily-notes-directory' if set, or in
`obsidian-inbox-directory' if set, or finally n `obsidian-directory' root."
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

;;;###autoload
(defun obsidian-jump ()
  "Jump to Obsidian note."
  (interactive)
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

(defun obsidian--add-file (file)
  "Add a FILE to the files cache and update tags and aliases for the file."
  (when (not (gethash file obsidian--vault-cache))
    (puthash file (make-hash-table :test 'equal :size 3) obsidian--vault-cache))
  (obsidian--update-file-metadata file))

(defun obsidian--remove-file (file)
  "Remove FILE from the files cache and update tags and aliases accordingly."
  (-map #'obsidian--remove-alias (obsidian--mapped-aliases file))
  (remhash file obsidian--vault-cache))

(defun obsidian--update-on-save ()
  "Used as a hook to update the vault cache when a file is saved."
  (when (obsidian--file-p (buffer-file-name))
    (obsidian--add-file (buffer-file-name))))

(defun obsidian--vault-directories ()
  "Provide a list of the directories in the Obsidian vault."
  (let* ((dict (make-hash-table :test 'equal))
         (_ (-map (lambda (d)
                    (puthash (file-relative-name d obsidian-directory) d dict))
                  (obsidian-directories))))
    dict))

;;;###autoload
(defun obsidian-move-file ()
  "Move current note to another directory."
  (interactive)
  (when (not (obsidian--file-p (buffer-file-name)))
    (user-error "Current file is not an obsidian-file"))
  (let* ((old-file-path (buffer-file-name))
         (dict (obsidian--vault-directories))
         (choice (completing-read "Move to: " (hash-table-keys dict)))
         (new-file-directory (file-name-as-directory (gethash choice dict)))
         (new-file-path (expand-file-name (file-name-nondirectory old-file-path) new-file-directory)))
    (when (equal new-file-path old-file-path)
      (user-error "File already exists at that location"))
    (rename-file old-file-path new-file-directory)
    (write-file new-file-path)
    (obsidian--remove-file old-file-path)
    (message "Moved to %s" new-file-path)))

(defun obsidian--prepare-file-path (s)
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
  "Open file F, offering a choice if multiple files match F.

If ARG is set, the file will be opened in other window."
  (let* ((all-files (->> (obsidian-files) (-map #'obsidian--file-relative-name)))
         (matches (obsidian--match-files f all-files))
         (file (cl-case (length matches)
                 (0 (obsidian--prepare-new-file-from-rel-path (obsidian--maybe-in-same-dir f)))
                 (1 (car matches))
                 (t
                  (let ((choice (completing-read "Jump to: " matches)))
                    choice))))
         (path (obsidian--expand-file-name file)))
    (if arg (find-file-other-window path) (find-file path))))

(defun obsidian-find-point-in-file (f p &optional arg)
  "Open file F at point P, offering a choice if multiple files match F.

If ARG is set, the file will be opened in other window."
  (obsidian-find-file f arg)
  (goto-char p))

(defun obsidian--maybe-in-same-dir (f)
  "If `/' in F, return F, otherwise with buffer, relative to the buffer."
  (if (s-contains-p "/" f)
      f
    (if obsidian-wiki-link-create-file-in-inbox
        f
      (concat (file-relative-name (file-name-directory (buffer-file-name)) obsidian-directory) "/" f))))

(defun obsidian-wiki-link-p ()
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
  "Remove section S from file path.
From `filename#section' keep only the `filename'."
  (replace-regexp-in-string "#.*$" "" s))

(defun obsidian-wiki->normal (f)
  "Add extension to wiki link F if none."
  (if (file-name-extension f)
      f
    (s-concat (obsidian--remove-section f) ".md")))

(defun obsidian-follow-wiki-link-at-point (&optional arg)
  "Find Wiki Link at point. Opens wiki links in other window if ARG is non-nil."
  (interactive "P")
  (thing-at-point-looking-at markdown-regex-wiki-link)
  (let* ((url (s-trim (if markdown-wiki-link-alias-first
                          (or (match-string-no-properties 5)
                              (match-string-no-properties 3))
                        (match-string-no-properties 3)))))
    (if (s-contains-p ":" url)
        (browse-url url)
      (let ((prepped-path (-> url
                              obsidian--prepare-file-path
                              obsidian-wiki->normal)))
        (push (point-marker) obsidian--jump-list)
        (obsidian-find-point-in-file 0 arg prepped-path)))))

(defun obsidian-follow-markdown-link-at-point (&optional arg)
  "Find and follow markdown link at point.
Opens markdown links in other window if ARG is non-nil.."
  (interactive "P")
  (let ((normalized (s-replace "%20" " " (markdown-link-url))))
    (if (s-contains-p ":" normalized)
        (browse-url normalized)
      (let ((prepped-path (obsidian--prepare-file-path normalized)))
        (push (point-marker) obsidian--jump-list)
        (obsidian-find-point-in-file prepped-path 0 arg )))))

(defun obsidian-follow-backlink-at-point ()
  "Open the file pointed to by the backlink and move to the linked location."
  (let* ((fil (get-text-property (point) 'obsidian--file))
         (pos (get-text-property (point) 'obsidian--position)))
    (when obsidian--debug-messages
        (message "Visiting file %s at position %s" fil pos))
    (find-file-other-window fil)
    (goto-char pos)))

(defun obsidian-backlink-p ()
  "Check if thing at point represents a backlink."
  (and (get-text-property (point) 'obsidian--file)
       (get-text-property (point) 'obsidian--position)))

(defun obsidian-follow-toc-link-at-point ()
  "Move point to section of note pointed to by table of contents links."
  (when (functionp 'markdown-toc-follow-link-at-point)
    (push (point-marker) obsidian--jump-list)
    (markdown-toc-follow-link-at-point)))

(defun obsidian-toc-link-p ()
  "Check if thing at point represent a table of contents."
  (and (functionp 'markdown-toc-follow-link-at-point)
       (markdown-link-p)
       (s-starts-with-p "#" (markdown-link-url))))

;;;###autoload
(defun obsidian-jump-back ()
  "Jump backward to previous location."
  (interactive)
  (if-let ((jump-marker (pop obsidian--jump-list)))
      (progn
        (pop-to-buffer (marker-buffer jump-marker))
        (goto-char jump-marker))
    (message "No previous location to jump to.")))

;;;###autoload
(defun obsidian-follow-link-at-point (&optional arg)
  "Follow thing at point if possible, such as a reference link or wiki link.
Opens inline and reference links in a browser.  Opens wiki links
to other files in the current window, or another window if ARG is non-nil.
See `markdown-follow-link-at-point' and `markdown-follow-wiki-link-at-point'."
  (interactive "P")
  (cond ((obsidian-toc-link-p)
         (obsidian-follow-toc-link-at-point))
        ((markdown-link-p)
         (obsidian-follow-markdown-link-at-point arg))
        ((obsidian-wiki-link-p)
         (obsidian-follow-wiki-link-at-point arg))
        ((obsidian-backlink-p)
         (obsidian-follow-backlink-at-point))))

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

;; TODO: Search for filename only as well as filename with relative subdir(s)
(defun obsidian--file-backlinks (file)
  "Return a hashtable of backlinks to FILE.

The variables used for retrieving links are as follows:

host - host file; the one that includes the link.  full path filename
targ - target file being pointed to by the host link, name and extension only
meta - metadata hashtable
lmap - hashmap of links from meta
link - link target from links hashmap
info - info list for target link from links hashmap

The files cache has the following structure:
  {filepath: {tags:    (tag list)
              aliases: (alias list)
              links:   {linkname: (link info list)}}}"
  (let* ((filename (file-name-nondirectory file))
         (targ filename)
         (resp (make-hash-table :test 'equal)))
    (maphash
     (lambda (host meta)
       (when-let ((links-map (gethash 'links meta)))
         (maphash
          (lambda (link info)
            (when (equal link targ)
              (puthash host info resp)))
          links-map)))
     obsidian--vault-cache)
    resp))

(defun obsidian-apply-template (template-filename)
  "Apply the template from TEMPLATE-FILENAME for the current buffer.
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

(defun obsidian--backlinks-completion-fn (hmap)
  "Completion function to show file path and link text from HMAP."
  (let* ((obsidian--backlinks-alist
          (ht-map (lambda (k v)
                    (cons (obsidian--file-relative-name k) (nth 2 v)))
                  hmap)))
    (completing-read
     "Backlinks: "
     (lambda (str pred flag)
       (if (eq flag 'metadata)
           '(metadata (annotation-function
                       lambda (str) (concat "\tlink text: " (cdr (assoc str obsidian--backlinks-alist)))))
         (all-completions str (mapcar #'car obsidian--backlinks-alist) pred))))))

(defun obsidian-backlinks (&optional file)
  "Return a backlinks hashmap for FILE."
  (let* ((filepath (or file (buffer-file-name)))
         (filename (file-name-nondirectory filepath))
         (linkmap (obsidian--file-backlinks filename)))
    linkmap))

;;;###autoload
(defun obsidian-backlink-jump (&optional file)
  "Select a backlink to this FILE and follow it."
  (interactive)
  (let ((linkmap (obsidian-backlinks file)))
    (if (> (length (hash-table-keys linkmap)) 0)
        (let* ((choice (obsidian--backlinks-completion-fn linkmap))
               (target (obsidian--expand-file-name choice))
               (link-info (gethash target linkmap)))
          (find-file target)
          (goto-char (car link-info)))
      (message "No backlinks found."))))

;;;###autoload
(defun obsidian-search ()
  "Search Obsidian vault for input."
  (interactive)
  (let* ((query (-> (read-from-minibuffer "Search query or regex: ")))
         (results (obsidian--grep query)))
    (message (s-concat "Found " (pp-to-string (length results)) " matches"))
    (let ((choice (completing-read "Select file: " results)))
      (obsidian-find-point-in-file choice 0))))

;;;###autoload
(defun obsidian-find-tag ()
  "Find all notes with a tag."
  (interactive)
  (let* ((taghash (obsidian-tags-ht))
         (tag (completing-read "Select tag: " (->> (hash-table-keys taghash)
                                                   (-sort 'string-lessp))))
         (results (gethash tag taghash))
         (choice (completing-read "Select file: " results)))
    (obsidian-find-point-in-file choice 0)))

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
    ("t" obsidian-find-tag)
    ("u" obsidian-update)
    ("w" obsidian-insert-wikilink :color blue)))

;;;###autoload
(define-globalized-minor-mode global-obsidian-mode obsidian-mode obsidian-enable-minor-mode)

(add-hook 'after-save-hook #'obsidian--update-on-save)

(defun obsidian-idle-timer ()
  "Wait until Emacs is idle to call update."
  (when obsidian--debug-messages
      (message "Update timer triggered at %s" (format-time-string "%H:%M:%S")))
  (run-with-idle-timer obsidian-update-idle-wait nil #'obsidian-update))

(when obsidian-use-update-timer
  (setq obsidian--update-timer
        (run-with-timer 0 obsidian-cache-expiry 'obsidian-idle-timer)))

(defun obsidian-stop-update-timer ()
  "Stop the background process that periodically refreshes the cache."
  (interactive)
  (cancel-timer obsidian--update-timer))

;;
;; Backlinks Panel
;;
(defcustom obsidian-backlinks-panel-position 'right
  "Position of treemacs buffer.

Valid values are
 * `right',
 * `left'."
  :type '(choice (const right)
                 (const left))
  :group 'backlinks-window)

(defcustom obsidian-backlinks-panel-width 75
  "Width of the backlinks window."
  :type 'integer
  :group 'backlinks-window)

(defcustom obsidian-backlinks-buffer-name "*backlinks*"
  "Name to use for the obsidian backlinks buffer."
  :type 'string
  :group 'backlinks-window)

(defcustom obsidian-backlinks-show-vault-path t
  "If t, show path relative to Obsidian vault, otherwise only show file name."
  :type 'boolean
  :group 'backlinks-window)

(defcustom obsidian-backlinks-filename-proportion 0.45
  "Proportion of space to be used to display the file, the rest
being used for the link text.
Setting a value of 1.0+ will cause 2 lines to be used per backlink with
the filename on the first line and the link text on the line below."
  :type 'float
  :group 'backlinks-window)

(defun obsidian--backlinks-format ()
  "Return a format string based on current `obsidian-backlinks-panel-width'."
  (if (< obsidian-backlinks-filename-proportion 1.0)
      (format "%%-%ds%%s\n"
              (ceiling (* obsidian-backlinks-panel-width
                          obsidian-backlinks-filename-proportion)))
    "%s\n - %s\n"))

(defun obsidian--get-local-backlinks-window (&optional frame)
  "Return window if backlinks window is visible in FRAME, nil otherwise.

Inspired by `treemacs-get-local-window' in `treemacs-scope.el'."
  (let ((search-frame (or frame (selected-frame))))
    (->> (window-list search-frame)
         (--first (->> it
                       (window-buffer)
                       (buffer-name)
                       (s-starts-with? obsidian-backlinks-buffer-name))))))

(defun obsidian--get-all-backlinks-windows ()
  "Return a list of all backlinks windows from all frames."
  (-non-nil (seq-map #'obsidian--get-local-backlinks-window (frame-list))))

(defun obsidian-backlinks-window ()
  "Visit backlinks buffer if not currently active or return to previous."
  (interactive)
  (if obsidian-backlinks-mode
      (if (equal (buffer-name) obsidian-backlinks-buffer-name)
          (progn
            (pop obsidian--jump-list)
            (select-window (get-mru-window (selected-frame) nil :not-selected)))
        (if-let ((bakbuf (get-buffer obsidian-backlinks-buffer-name)))
            (progn
              (push (point-marker) obsidian--jump-list)
              (pop-to-buffer bakbuf))
          (obsidian--populate-backlinks-buffer)))
    (obsidian-backlink-jump)))

(defun obsidian--backlinks-set-width (width)
  "Set the width of the backlinks buffer to WIDTH."
  (unless (one-window-p)
    (let* ((bakbuf (get-buffer obsidian-backlinks-buffer-name))
           (win (get-buffer-window obsidian-backlinks-buffer-name))
           (win-width (window-width win))
           (new-width (max width window-safe-min-width)))
      ;; (message "win-width: %d\tnew-width: %d" win-width new-width)
      (save-excursion
        (set-buffer bakbuf)
        (setq window-size-fixed nil)
        (window-resize win (- new-width win-width) t)
        (message "> :: adjusted %d to %d (win-width: %d\tnew-width: %d)"
                 (- new-width win-width) (window-width win) win-width new-width)
        (setq window-size-fixed 'width))
      (setq obsidian-backlinks-panel-width new-width)
      (obsidian--populate-backlinks-buffer 'force))))

;; TODO: Does this arg make sense here?
(defun obsidian-backlinks-set-width (&optional arg)
  "Select a new value for `obsidian-backlinks-panel-width'.
With a prefix ARG simply reset the width of the treemacs window."
  (interactive "P")
  (unless arg
    (setq obsidian-backlinks-panel-width
          (->> obsidian-backlinks-panel-width
               (format "New Width (current = %s): ")
               (read-number))))
  (obsidian--backlinks-set-width obsidian-backlinks-panel-width))

;; TODO: If a user has backlinks mode t in their config, a backlinks panel
;;       should be open the first time they visit a markdown file
(defun obsidian-open-backlinks-panel ()
  "Create a dedicated panel to display the backlinks buffer.

Inspired by treemacs. See `treemacs--popup-window' in `treemacs-core-utils.el'
for an example of using `display-buffer-in-side-window'."
  (interactive)
  (let ((bakbuf (get-buffer-create obsidian-backlinks-buffer-name)))
    (save-excursion
      (set-buffer bakbuf)
      (setq window-size-fixed 'width))
    (display-buffer-in-side-window
     bakbuf
     `((side . ,obsidian-backlinks-panel-position)
       (window-width . ,obsidian-backlinks-panel-width)
       (slot . -1)  ;; because treemacs--popup-window included this
       (dedicated . t)))))

(defun obsidian-close-backlinks-panel ()
  "Close local window used for dedicated backlinks panel."
  (interactive)
  (delete-window (obsidian--get-local-backlinks-window)))

(defun obsidian-close-all-backlinks-panels ()
  "Close all windows used for dedicated backlinks panels."
  (when-let ((wins (obsidian--get-all-backlinks-windows)))
    (seq-map #'delete-window wins)
    (balance-windows)))

(defun obsidian-toggle-backlinks-panel ()
  "Create backlinks panel if it doesn't exist, close it otherwise.

Returns t if a panel was created, nil if closed."
  (interactive)
  (if (obsidian--get-local-backlinks-window)
      (progn
        (obsidian-close-backlinks-panel)
        nil)
    (progn
      (obsidian-open-backlinks-panel)
      (obsidian-backlinks-mode t)
      t)))

(defun obsidian--link-with-props (k v)
  "Create a propertized link and link text string from K and V.

K is the file name that contains the link.
V is the list object associated with the link as returned
by `markdown-link-at-pos'."
  (let* ((rel-file (obsidian--file-relative-name k))
         (link-txt (nth 2 v))
         (ptxt (format (obsidian--backlinks-format)
                       (propertize rel-file
                                   'face 'markdown-metadata-key-face
                                   'obsidian--file k
                                   'obsidian--position (nth 0 v))
                       (propertize link-txt 'face 'markdown-metadata-value-face))))
    (insert ptxt)))

(defun obsidian--file-backlinks-displayed-p (&optional file)
  "Return t if the backlinks panel is showing the backlinks for FILE, else nil.

FILE is the full path to an obsidian file."
  (let* ((file-path (or file (buffer-file-name)))
         (bakbuf (get-buffer obsidian-backlinks-buffer-name))
         (file-prop (get-text-property 1 'obsidian-mru-file bakbuf)))
    (equal file-path file-prop)))

(defun obsidian--populate-backlinks-buffer (&optional force)
  "Populate backlinks buffer with backlinks for current Obsidian file.

The backlinks buffer will not be updated if it's already showing the
backlinks for the current buffer unless FORCE is non-nil."
  (interactive)
  (unless (and (obsidian--file-backlinks-displayed-p) (not force))
    (when (and obsidian-mode (obsidian--file-p))
      (let* ((file-path (buffer-file-name))
             (vault-path (obsidian--file-relative-name file-path))
             (backlinks (obsidian-backlinks))
             (file-str (if obsidian-backlinks-show-vault-path
                           vault-path
                         (file-name-base file-path))))
        (with-current-buffer (get-buffer obsidian-backlinks-buffer-name)
          (erase-buffer)
          (insert (propertize (format "# %s\n" file-str)
                              'face 'markdown-header-face
                              'obsidian-mru-file file-path))
          (insert (propertize
                   (format "%s\n" (make-string (- obsidian-backlinks-panel-width 2) ?-))
                   'face 'markdown-hr-face))
          (maphash 'obsidian--link-with-props backlinks)
          ;; Allows for using keybindings for obsidian-open-link
          (obsidian-mode t)
          ;; Put cursor on the line of the first link
          (goto-char (point-min))
          (forward-line 2)
          (set-window-point
           (get-buffer-window obsidian-backlinks-buffer-name)
           (point)))))))

(defun obsidian-backlinks-count-map ()
  "Return a hashmap with a backlinks count for each file.

The key is a full file path and the value with be an integer count
of the number of backlinks pointing to that file."
  (let* ((obs-files (hash-table-keys obsidian--vault-cache))
         (num-files (length obs-files))
         (bakmap (make-hash-table :test 'equal :size num-files)))
    (seq-map (lambda (f)
               (let* ((backlinks (obsidian-backlinks f))
                      (count (length (hash-table-keys backlinks)))
                      ;; (rel-file (obsidian--file-relative-name f))
                      (rel-file f))
                 (puthash rel-file count bakmap)))
             obs-files)
    bakmap))

;;;###autoload
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
    (obsidian-open-backlinks-panel)
    (obsidian--populate-backlinks-buffer)
    (add-hook 'buffer-list-update-hook #'obsidian--populate-backlinks-buffer)
    (if (boundp 'eyebrowse-post-window-switch-hook)
        (remove-hook 'eyebrowse-post-window-switch-hook #'obsidian-close-all-backlinks-panels)))
   (t
    ;; mode was turned off (or we refused to turn it on)
    (remove-hook 'buffer-list-update-hook #'obsidian--populate-backlinks-buffer)
    (obsidian-close-all-backlinks-panels)
    (if (boundp 'eyebrowse-post-window-switch-hook)
        (add-hook 'eyebrowse-post-window-switch-hook #'obsidian-close-all-backlinks-panels)))))






(provide 'obsidian)
;;; obsidian.el ends here
