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

;; TODO: Implement this functionality
(defcustom obsidian-backlinks-panel-position 'right
  "Position of treemacs buffer.

Valid values are
 * `left',
 * `right'"
  :type '(choice (const left)
                 (const right))
  :group 'backlinks-window)

(defcustom obsidian-backlinks-panel-width 75
  "Width of the backlinks window."
  :type 'integer
  :group 'backlinks-window)

(defcustom obsidian-backlinks-buffer-name "*backlinks*"
  "Name to use for the obsidian backlinks buffer."
  :type 'string
  :group 'backlinks-window)

;; "%-33s%-33s\n"
;; TODO: Does this update if obsidian-backlinks-panel-width is updated?
(defcustom obsidian-backlink-format
  (format "%%-%ds%%-%ds\n"
          (ceiling obsidian-backlinks-panel-width 2)
          (floor obsidian-backlinks-panel-width 2))
  "String format to use for displaying backlinks and link text."
  :type 'string
  :group 'backlinks-window)

(defun obsidian--link-with-props (k v)
  "Create a propertized link and link text string from a link list v.

k is the file name that contains the link.
v is the list object associated with the link as returned
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

(defun obsidian-backlinks-other-window ()
  "Create a backlinks buffer for the current buffer and show in other window."
  (interactive)
  (when (and obsidian-mode (obsidian--file-p))
    (let* ((file-path (buffer-file-name))
           (backlinks (obsidian--backlinks))
           (base (file-name-base (buffer-file-name)))
           (buffer "*backlinks*"))
      (when backlinks
        (with-current-buffer-window buffer
            nil
            nil
          (progn
            (insert (propertize (format "# %s\n\n" file-path) 'face 'markdown-header-face))
            (insert (propertize (format obsidian-backlink-format "File Name" "Link Text") 'face 'markdown-header-face))
            (insert (propertize "-----------------------------------------------------\n" 'face 'markdown-hr-face))
            (maphash 'obsidian--link-with-props backlinks)
            (obsidian-mode t)
            (goto-line 5)))))))


;; (add-hook 'buffer-list-update-hook #'obsidian-backlinks-other-window)
;; (remove-hook 'buffer-list-update-hook #'obsidian-backlinks-other-window)

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
    (obsidian-backlinks-other-window)
    (add-hook 'buffer-list-update-hook #'obsidian-backlinks-other-window))
   (t
    ;; mode was turned off (or we refused to turn it on)
    (remove-hook 'buffer-list-update-hook #'obsidian-backlinks-other-window)
    (if (get-buffer obsidian-backlinks-buffer-name)
        (kill-buffer obsidian-backlinks-buffer-name)))))



(global-set-key (kbd "<f5>") (lambda () (interactive (message (format "%s" (buffer-list))))))
(global-set-key (kbd "<f6>") 'obsidian-backlinks-window)
(global-set-key (kbd "<f7>") (lambda () (interactive) (message "%s" (text-properties-at (point)))))
(global-set-key (kbd "<f8>") 'obsidian-backlinks-mode)
;; (define-key obsidian-mode-map (kbd "<f6>") 'obsidian-backlinks-window)



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

;; TODO: This docstring is taken right from org-indent-mode
;;;###autoload
(define-minor-mode obsidian-indent-mode
  "When active, indent text according to outline structure.

Internally this works by adding `line-prefix' and `wrap-prefix'
properties, after each buffer modification, on the modified zone.

The process is synchronous.  Though, initial indentation of
buffer, which can take a few seconds on large buffers, is done
during idle time."
  :global t
  :lighter " OInd")


(provide 'obsidian-ext)
;; obsidian-ext ends here
