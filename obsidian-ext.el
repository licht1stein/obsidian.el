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
 * `right',
 ;; * `top',
 ;; * `bottom',
 * `left'."
  :type '(choice (const right)
                 ;; (const top)
                 ;; (const bottom)
                 (const left))
  :group 'backlinks-window)

(defcustom obsidian-backlinks-panel-width 55
  "Width of the backlinks window."
  :type 'integer
  :group 'backlinks-window)

(defcustom obsidian-backlinks-buffer-name "*backlinks*"
  "Name to use for the obsidian backlinks buffer."
  :type 'string
  :group 'backlinks-window)

(defcustom obsidian-backlinks-show-full-file-path t
  "If t, show full path for linked file, otherwise only show file name."
  :type 'boolean
  :group 'backlinks-window)

;; "%-33s%-33s\n"
;; TODO: Does this update if obsidian-backlinks-panel-width is updated?
(defcustom obsidian-backlink-format
  (format "%%-%ds%%-%ds\n"
          (ceiling (* obsidian-backlinks-panel-width 0.4))
          (floor (* obsidian-backlinks-panel-width 0.6))
          ;; (ceiling obsidian-backlinks-panel-width 2)
          ;; (floor obsidian-backlinks-panel-width 2)
          )
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


(defun obsidian--get-local-backlinks-window (&optional frame)
  "Return window if backlinks window is visible in FRAME, nil otherwise."
  (let ((search-frame (or frame (selected-frame))))
    (->> (window-list search-frame)
         (--first (->> it  ;; TODO: why is 'it' not a void variable?
                       (window-buffer)
                       (buffer-name)
                       (s-starts-with? obsidian-backlinks-buffer-name))))))

(defun obsidian--get-all-backlinks-windows ()
  "Return a list of all backlinks windows from all frames."
  ;; TODO: This does not include windows in other eyebrowse windows
  (-non-nil (seq-map #'obsidian--get-local-backlinks-window (frame-list))))

(defun obsidian-backlinks-window ()
  "Visit backlinks buffer if not currently active or return to previous."
  (interactive)
  (if obsidian-backlinks-mode
      (if (equal (buffer-name) obsidian-backlinks-buffer-name)
          ;; vs pop-to-buffer ?
          (select-window (get-mru-window (selected-frame) nil :not-selected))
        (if (get-buffer obsidian-backlinks-buffer-name)
            (pop-to-buffer obsidian-backlinks-buffer-name)
          (obsidian-backlinks-other-window)))
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
  "Close all windows used for dedicated backlinks panel."
  (interactive)
  ;; (delete-window (obsidian--get-local-backlinks-window))
  (seq-map #'delete-window (obsidian--get-all-backlinks-windows)))


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
    (obsidian-open-backlinks-panel)
    ;; (obsidian-backlinks-other-window)
    (add-hook 'buffer-list-update-hook #'obsidian-backlinks-other-window))
   (t
    ;; mode was turned off (or we refused to turn it on)
    (remove-hook 'buffer-list-update-hook #'obsidian-backlinks-other-window)
    ;; (if (get-buffer obsidian-backlinks-buffer-name)
    ;;     (kill-buffer obsidian-backlinks-buffer-name))
    (obsidian-close-backlinks-panel))))






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
