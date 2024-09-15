;;; obsidian-ext.el --- Extention to obsidian.el -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (c) 2024 Joe Reinhart <joseph.reinhart@gmail.com>

;; Author: Joe Reinhart
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

(setq backlink-format "%-33s%-33s\n")

(defun link-with-props (k v)
  (let* ((rel-file (obsidian--file-relative-name k))
         (link-txt (nth 2 v))
         (ptxt (format backlink-format
                       (propertize rel-file
                                   'face 'markdown-metadata-key-face
                                   'obsidian--file k
                                   'obsidian--position (nth 0 v))
                       (propertize link-txt 'face 'markdown-metadata-value-face))))
    ;; (message "%s: %s" k v)
    (insert ptxt)))

(defun obsidian-backlinks-other-window ()
  (interactive)
  (if (and obsidian-mode (obsidian--file-p))
      (let* ((obs-name (file-name-sans-extension (buffer-name)))
             (backlinks (obsidian--backlinks))
             (base (file-name-base (buffer-file-name)))
             (buffer "*backlinks*"))
        (when backlinks
          (with-current-buffer-window buffer
              nil
              nil
            (progn
              (insert (propertize (format "# %s\n\n" obs-name) 'face 'markdown-header-face))
              (insert (propertize (format backlink-format "File Name" "Link Text") 'face 'markdown-header-face))
              (insert (propertize "-----------------------------------------------------\n" 'face 'markdown-hr-face))
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
