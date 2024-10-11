;;; obsidian-ext.el --- Extention to obsidian.el -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (c) 2024 Joe Reinhart <joseph.reinhart@gmail.com>

;; Author: Joe Reinhart
;; URL: https://github.com/licht1stein/obsidian.el
;; Keywords: obsidian, pkm, convenience
;; Version: 1.4.4
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

;; TODO: markdown-next-link does not trigger markdown-eldoc-function
;;       - this would show the hidden link in the echo area

;; TODO: Need a function to find and (help) fix *conflicte* files

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

;; "%-33s%-33s\n"
;; TODO: Does this update if obsidian-backlinks-panel-width is updated?
(defcustom obsidian-backlink-format
  (format "%%-%ds%%s\n" (ceiling (* obsidian-backlinks-panel-width 0.45)))
  "String format to use for displaying backlinks and link text."
  :type 'string
  :group 'backlinks-window)

(defun obsidian--get-local-backlinks-window (&optional frame)
  "Return window if backlinks window is visible in FRAME, nil otherwise."
  (let ((search-frame (or frame (selected-frame))))
    (->> (window-list search-frame)
         (--first (->> it  ;; TODO: why is 'it' not a void variable?
                       (window-buffer)
                       (buffer-name)
                       (s-starts-with? obsidian-backlinks-buffer-name))))))



(defun obsidian--backlinks-window-list (eyebrowse-config)
  (get-buffer-window-list)
  )

(defun obsidian--eyebrowse-configs ()
  "Return a list of window configs used by eyebrowse.

The function eyebrowse-renumber-window-configs provided the logic.

OTHER POSSIBILITIES:
  - eyebrowse--walk-window-config
  - eyebrowse--get 'window-configs
  - eyebrowse--set 'window-configs <modified-configs>

"
  (when eyebrowse-mode
    (mapcar 'car (eyebrowse--get 'window-configs))))

;; (seq-map (lambda (cfg) (get-buffer-window-list "*backlinks*" nil t)) (eyebrowse--get 'window-configs))
;; eyebrowse--set

;; (message "%s" (eyebrowse--get 'window-configs))

;; (let ((cfg (car (eyebrowse--get 'window-configs))))
;;   (message "%s" (type-of cfg))
;;   (message "%s" (length cfg))
;;   (eyebrowse--walk-window-config cfg (lambda (w) (message "Type of w: %s" w)))
;;   )



(defun obsidian--get-all-backlinks-windows ()
  "Return a list of all backlinks windows from all frames."
  ;; TODO: This does not include windows in other eyebrowse windows
  (-non-nil (seq-map #'obsidian--get-local-backlinks-window (frame-list))))

(defun obsidian-backlinks-window ()
  "Visit backlinks buffer if not currently active or return to previous."
  (interactive)
  (if obsidian-backlinks-mode
      (if (equal (buffer-name) obsidian-backlinks-buffer-name)
          (select-window (get-mru-window (selected-frame) nil :not-selected))
        (if-let ((bakbuf (get-buffer obsidian-backlinks-buffer-name)))
            (pop-to-buffer bakbuf)
          (obsidian-populate-backlinks-buffer)))
    (obsidian-backlink-jump)))

(defun obsidian--backlinks-set-width (width)
  "Set the width of the backlinks buffer to WIDTH."
  (unless (one-window-p)
    ;; (with-current-buffer
    ;; (get-buffer (buffer-name))
    ;; obsidian-backlinks-bufer-name
    ;; (save-current-buffer
    (with-current-buffer-window obsidian-backlinks-buffer-name
        nil
        nil
      (let ((window-size-fixed)
            (w (max width window-safe-min-width)))
        (cond
         ((> (window-width) w)
          (shrink-window-horizontally  (- (window-width) w)))
         ((< (window-width) w)
          (enlarge-window-horizontally (- w (window-width)))))))))

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

;; TODO: See treemacs--popup-window in treemacs-core-utils.el
;;       for an example of using display-buffer-in-side-window.
(defun obsidian-open-backlinks-panel ()
  "Create a dedicated panel to display the backlinks buffer."
  (interactive)
  (display-buffer-in-side-window
   (get-buffer-create "*backlinks*")
   `((side . ,obsidian-backlinks-panel-position)
     (window-width . ,obsidian-backlinks-panel-width)
     (slot . -1)  ;; because treemacs--popup-window included this
     (dedicated . t))))

(defun obsidian-close-backlinks-panel ()
  "Close local window used for dedicated backlinks panel."
  (interactive)
  (delete-window (obsidian--get-local-backlinks-window)))

;; TODO: Doesn't work for hidden eyebrowse windows
(defun obsidian-close-all-backlinks-panels ()
  "Close all windows used for dedicated backlinks panels."
  (seq-map #'delete-window (obsidian--get-all-backlinks-windows)))

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
by markdown-link-at-pos."
  (let* ((rel-file (obsidian--file-relative-name k))
         (link-txt (nth 2 v))
         (ptxt (format obsidian-backlink-format
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

(defun obsidian-populate-backlinks-buffer ()
  "Populate backlinks buffer with backlinks for current Obsidian file."
  (interactive)
  (unless (obsidian--file-backlinks-displayed-p)
    (when (and obsidian-mode (obsidian--file-p))
      (let* ((file-path (buffer-file-name))
             (vault-path (obsidian--file-relative-name file-path))
             (backlinks (obsidian--backlinks))
             (file-str (if obsidian-backlinks-show-vault-path
                           vault-path
                         (file-name-base file-path))))
        (with-current-buffer (get-buffer obsidian-backlinks-buffer-name)
          (erase-buffer)
          (insert (propertize (format "# %s\n\n" file-str)
                              'face 'markdown-header-face
                              'obsidian-mru-file file-path))
          (insert (propertize "----------------------------------------------\n"
                              'face 'markdown-hr-face))
          (maphash 'obsidian--link-with-props backlinks)
          (obsidian-mode t)  ;; Allows for using keybindings for obsidian-open-link
          (goto-line 4)
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
               (let* ((backlinks (obsidian--backlinks f))
                      (count (length (hash-table-keys backlinks)))
                      ;; (rel-file (obsidian--file-relative-name f))
                      (rel-file f))
                 (puthash rel-file count bakmap)))
             obs-files)
    bakmap))

(defun obsidian-backlinks-file-compare (&optional sort)
  "Show a list of all files and their corresponding backlinks count.

Non-nil SORT will return files sorted in order of increasing backlink count."
  (interactive)
  (if (y-or-n-p "This is a resource intensive function. Proceed?")
      (let* ((bakmap (obsidian-backlinks-count-map))
             (max-len (apply 'max (->> (hash-table-keys bakmap)
                                       (seq-map 'obsidian--file-relative-name)
                                       (seq-map 'length))))
             (bakmap (if sort (obsidian--sort-map-by-values bakmap) bakmap)))
        (with-current-buffer (get-buffer-create "*backlinks-info*")
          (erase-buffer)
          (insert (propertize "File Name\t\tNumber of Backlinks\n" 'face 'markdown-header-face))
          (insert (propertize "------------------------------------\n" 'face 'markdown-hr-face))
          (maphash (lambda (k v)
                     (insert (format (format "%%-%ds%%s\n" (1+ max-len))
                                     (propertize (obsidian--file-relative-name k)
                                                 'face 'markdown-metadata-key-face
                                                 'obsidian--file k 'obsidian--position 0)
                                     (propertize (format "%s" v)
                                                 'face 'markdown-metadata-value-face))))
                   bakmap)
          (obsidian-mode t)
          (message "Processed %d files (max key length: %d)"
                   (length (hash-table-keys bakmap)) max-len)))))

(defun obsidian-files-without-backlinks ()
  "Show a list of all file that do not have any backlinks."
  (interactive)
  (if (y-or-n-p "This is a resource intensive function. Proceed?")
      (let* ((bakmap (obsidian-backlinks-count-map))
             (resp '()))
        (maphash (lambda (k v) (if (= 0 v) (add-to-list 'resp k))) bakmap)
        (with-current-buffer (get-buffer-create "*backlinks-info*")
          (erase-buffer)
          (insert (propertize "Files without backlinks:\n" 'face 'markdown-header-face))
          (insert (propertize "----------------------------------------\n" 'face 'markdown-hr-face))
          (seq-map (lambda (f)
                     (insert (propertize (format "%s\n" (obsidian--file-relative-name f))
                                         'face 'markdown-metadata-key-face
                                         'obsidian--file f
                                         'obsidian--position 0)))
                   resp)
          (obsidian-mode t))
        resp)))

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
    (obsidian-populate-backlinks-buffer)
    (add-hook 'buffer-list-update-hook #'obsidian-populate-backlinks-buffer))
   (t
    ;; mode was turned off (or we refused to turn it on)
    (remove-hook 'buffer-list-update-hook #'obsidian-populate-backlinks-buffer)
    (obsidian-close-all-backlinks-panels))))






;;
;; TODO
;;

(defun obsidian-indent-indent-buffer ()
  "Add indentation properties to the accessible part of the buffer."
  (interactive)
  (if (not t) ;; (derived-mode-p 'org-mode)
      (error "Not in Obsidian mode")
    (message "Setting buffer indentation.  It may take a few seconds...")
    (org-indent-remove-properties (point-min) (point-max))
    (org-indent-add-properties (point-min) (point-max))
    (message "Indentation of buffer set.")))


(provide 'obsidian-ext)
;; obsidian-ext ends here
