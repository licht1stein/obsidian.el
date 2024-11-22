;;; test-obsidian.el --- Obsidian Tests -*- coding: utf-8; lexical-binding: t; -*-
(require 'obsidian)
(require 'buttercup)

;; Using a relative path for obsidian-change-vault/obsidian-change-vault will
;; result in different values for obsidian-directory depending on the directory
;; of the most recently visited file
(defvar obsidian--test-dir (expand-file-name "./tests/test_vault"))
(defvar obsidian--test--original-dir (or obsidian-directory obsidian--test-dir))
(defvar obsidian--test--original-wiki-link-alias-first obsidian-wiki-link-alias-first)
(defvar obsidian--test--original-enable-wiki-links markdown-enable-wiki-links)
(defvar obsidian--test-number-of-tags 9)
(defvar obsidian--test-number-of-visible-tags 6)
(defvar obsidian--test-number-of-notes 11)
(defvar obsidian--test-number-of-visible-notes 9)
(defvar obsidian--test-number-of-visible-directories 2)
(defvar obsidian--test-visibility-cfg obsidian-include-hidden-files)

(defun obsidian-test--delete-all-test-files ()
  "Function to delete all files potentially left behind by tests."
  (delete-file (concat obsidian-directory "/foo.md"))
  (delete-file (concat obsidian-directory "/bar.md"))
  (delete-file (concat obsidian-directory "/inbox/foo.md"))
  (delete-file (concat obsidian-directory "/inbox/bar.md"))
  (delete-file (concat obsidian-directory "/subdir/foo.md"))
  (delete-file (concat obsidian-directory "/subdir/bar.md")))

(defun obsidian-test--jump-to-file (file)
  "FILE is a path relative to the obsidian vault."
  (let* ((obsidian-inbox-directory "inbox")
         (executing-kbd-macro t)
         (unread-command-events (listify-key-sequence (format "%s\n" file))))
    (call-interactively #'obsidian-jump)))

(describe "check path setting"
  (before-all (progn
                (setq obsidian-include-hidden-files t)
                (obsidian-change-vault obsidian--test-dir)))
  (after-all (progn
               (setq obsidian-include-hidden-files obsidian--test-visibility-cfg)
               (obsidian-change-vault obsidian--test--original-dir)))
  (it "set to current"
      (expect obsidian-directory :to-equal (expand-file-name obsidian--test-dir))
      (expect (length (ht-keys obsidian-vault-cache))
              :to-equal obsidian--test-number-of-notes)
      ;; Use empty directory as vault and verify change
      (let ((tmp-dir (make-temp-file "obs" t)))
        (obsidian-change-vault tmp-dir)
        (expect obsidian-directory :to-equal tmp-dir)
        (expect (length (ht-keys obsidian-vault-cache)) :to-equal 0))
      ;; Change vault to a non-existent directory
      (expect (obsidian-change-vault "/path/that/does/not/exist")
              :to-throw 'user-error)
      ;; Change vault path to test-dir and verify change
      (obsidian-change-vault obsidian--test-dir)
      (expect obsidian-directory :to-equal obsidian--test-dir)
      (expect (length (ht-keys obsidian-vault-cache))
              :to-equal obsidian--test-number-of-notes)))

(describe "obsidian-file-p"
  (before-all (obsidian-change-vault obsidian--test-dir))
  (after-all (obsidian-change-vault obsidian--test--original-dir))

  (it "include files right in vault"
    (expect (obsidian-file-p "./tests/test_vault/1.md") :to-be t))
  (it "include files in subdirs"
    (expect (obsidian-file-p "./tests/test_vault/subdir/1-sub.md") :to-be t))
  (it "exclude files in trash"
    (expect (obsidian-file-p "./tests/test_vault/.trash/trash.md") :to-be nil)))

(describe "obsidian list all visible files"
   (before-all (progn
                 (setq obsidian-include-hidden-files nil)
                 (obsidian-change-vault obsidian--test-dir)))
   (after-all (progn
                (setq obsidian-include-hidden-files obsidian--test-visibility-cfg)
                (obsidian-change-vault obsidian--test--original-dir)))

  (it "check visible file count"
    (expect (length (obsidian-files)) :to-equal obsidian--test-number-of-visible-notes)))

(describe "obsidian list all files including hidden files"
   (before-all (progn
                 (setq obsidian-include-hidden-files t)
                 (obsidian-change-vault obsidian--test-dir)))
   (after-all (progn
                (setq obsidian-include-hidden-files obsidian--test-visibility-cfg)
                (obsidian-change-vault obsidian--test--original-dir)))

  (it "check all files count"
    (expect (length (obsidian-files)) :to-equal obsidian--test-number-of-notes)))

(describe "obsidian-directories"
   (before-all (progn
                 (setq obsidian-include-hidden-files nil)
                 (obsidian-change-vault obsidian--test-dir)))
   (after-all (progn
                (setq obsidian-include-hidden-files obsidian--test-visibility-cfg)
                (obsidian-change-vault obsidian--test--original-dir)))

  (it "check directory count"
    (expect (length (obsidian-directories)) :to-equal
            obsidian--test-number-of-visible-directories)))

(describe "obsidian-remove-front-matter-front-string"
  (it "Remove front matter from string"
      (expect (obsidian-remove-front-matter-from-string "---\ntags: [foo]\n---\none\ntwo")
              :to-equal "one\ntwo"))
  (it "Return string when front matter isn't present"
      (expect (obsidian-remove-front-matter-from-string "---\none\ntwo")
              :to-equal "---\none\ntwo")
      (expect (obsidian-remove-front-matter-from-string "one\ntwo")
              :to-equal "one\ntwo")))

(describe "obsidian--find-tags-in-string"
  (before-all (obsidian-change-vault obsidian--test-dir))
  (after-all (obsidian-change-vault obsidian--test--original-dir))

  (it "find tags in string"
    (expect (length (obsidian--find-tags-in-string
                     "#foo bar #spam #bar-spam #spam_bar #foo+spam #foo=bar not tags #123 #+invalidtag"))
            :to-equal 6)
    (expect (obsidian--find-tags-in-string "---\ntags: \n---") :to-equal nil)
    (expect (obsidian--find-tags-in-string "---\ntags: one\n---") :to-equal nil)
    (expect (obsidian--find-tags-in-string "---\ntags: one two three\n---") :to-equal nil)
    (expect (obsidian--find-tags-in-string "---\ntags: one, two, three\n---") :to-equal nil)
    (expect (obsidian--find-tags-in-string "---\ntags: [one two three]\n---") :to-equal nil)
    (expect (obsidian--find-tags-in-string "---\ntags: [one #two three]\n---") :to-equal nil)
    (expect (obsidian--find-tags-in-string "---\ntags: one, #two, three---\n") :to-equal nil)
    (expect (obsidian--find-tags-in-string "---\ntags: [one, two, three]\n---")
            :to-equal '("one" "two" "three"))
    (expect (obsidian--find-tags-in-string "---\ntags:\n- one\n- two\n- three\n---\n")
            :to-equal '("one" "two" "three"))))

(describe "obsidian--find-aliases-in-string"
  (before-all (obsidian-change-vault obsidian--test-dir))
  (after-all (obsidian-change-vault obsidian--test--original-dir))
  (it "find aliases in string"
    (expect (obsidian--find-aliases-in-string "---\naliases: \n---")
            :to-equal nil)
    (expect (obsidian--find-aliases-in-string "---\naliases: [file1]\n---")
            :to-equal '("file1"))
    (expect (obsidian--find-aliases-in-string "---\naliases: [file1, file2]\n---")
            :to-equal '("file1" "file2"))
    (expect (obsidian--find-aliases-in-string "---\naliases:\n- file1\n- file2\n---")
            :to-equal '("file1" "file2"))))

(describe "obsidian-list-visible-tags"
  (before-all (progn
                (setq obsidian-include-hidden-files nil)
                (obsidian-change-vault obsidian--test-dir)))
  (after-all (progn
               (setq obsidian-include-hidden-files obsidian--test-visibility-cfg)
               (obsidian-change-vault obsidian--test--original-dir)))

  (it "find all tags in the vault"
    (expect (length (obsidian-tags)) :to-equal obsidian--test-number-of-visible-tags)))

(describe "obsidian list all tags including hidden tags"
  (before-all (progn
                (setq obsidian-include-hidden-files t)
                (obsidian-change-vault obsidian--test-dir)))
  (after-all (progn
               (setq obsidian-include-hidden-files obsidian--test-visibility-cfg)
               (obsidian-change-vault obsidian--test--original-dir)))

  (it "find all tags in the vault"
    (expect (length (obsidian-tags)) :to-equal obsidian--test-number-of-tags)))

(describe "obsidian-repopulate-cache"
  (before-all (progn
		(obsidian-change-vault obsidian--test-dir)
		(obsidian-clear-cache)))
  (after-all (obsidian-change-vault obsidian--test--original-dir))

  (it "check that tags var is empty before populate-cache"
    (expect (obsidian-tags) :to-be nil))
  (it "check tags are filled out after populate-cache"
    (expect (progn
	      (obsidian-repopulate-cache)
	      (length (obsidian-tags))) :to-equal obsidian--test-number-of-tags)))


(defvar-local obsidian--test-correct-front-matter "---
aliases: [AI, Artificial Intelligence]
tags: [one, two, three]
key4:
- four
- five
- six
---
")
(defvar obsidian--test-incorrect-front-matter--not-start-of-file
  (s-concat "# Header\n" obsidian--test-correct-front-matter))

(describe "obsidian-aliases"
  (before-all (obsidian-change-vault obsidian--test-dir))
  (after-all (obsidian-change-vault obsidian--test--original-dir))

  (it "check that front-matter is found"
    (expect (->> obsidian--test-correct-front-matter
                 obsidian--find-yaml-front-matter-in-string
                 (gethash 'aliases)) :to-equal ["AI" "Artificial Intelligence"]))

  (it "check that front-matter is ignored if not at the top of file"
    (expect (obsidian--find-yaml-front-matter-in-string
             obsidian--test-incorrect-front-matter--not-start-of-file) :to-equal nil))

  (it "check that front-matter in vault is correct"
    (let ((alias-list (obsidian-aliases)))
      (expect (length alias-list) :to-equal 6)
      (expect (seq-contains-p alias-list "2") :to-equal t)
      (expect (seq-contains-p alias-list "2-sub-alias") :to-equal t)
      (expect (seq-contains-p alias-list "complex file name") :to-equal t)
      (expect (seq-contains-p alias-list "alias-one-off") :to-equal t)
      (expect (seq-contains-p alias-list "alias1") :to-equal t)
      (expect (seq-contains-p alias-list "alias2") :to-equal t))))

(describe "obsidian--link-p"
  (it "non link"
    (expect (obsidian--link-p "not link") :to-equal nil))

  (it "wiki link"
    (expect (obsidian--link-p "[[foo.md]]") :to-equal t)
    (expect (obsidian--link-p "[[foo]]") :to-equal t)
    (expect (obsidian--link-p "[[foo|annotated link]]") :to-equal t))

  (it "markdown link"
    (expect (obsidian--link-p "[foo](bar)") :to-equal t)
    (expect (obsidian--link-p "[foo](bar.md)") :to-equal t)))

(describe "obsidian unique links count including wiki links"
   (before-all (progn
                 (setq markdown-enable-wiki-links t)
                 (setq obsidian-wiki-link-alias-first nil)
                 (obsidian-change-vault obsidian--test-dir)))
   (after-all (progn
                (setq markdown-enable-wiki-links obsidian--test--original-enable-wiki-links)
                (setq obsidian-wiki-link-alias-first
                      obsidian--test--original-wik-link-alias-first)
                (obsidian-change-vault obsidian--test--original-dir)))

   (it "1.md unique link count"
     (let* ((file (obsidian-expand-file-name "1.md"))
            (links (ht-get (ht-get obsidian-vault-cache file) 'links)))
       (expect (length (ht-keys links)) :to-equal 3)))

   (it "subdir/1.md unique link count"
     (let* ((file (obsidian-expand-file-name "subdir/1.md"))
            (links (ht-get (ht-get obsidian-vault-cache file) 'links)))
       (expect (length (ht-keys links)) :to-equal 1)))

   (it "2.md unique link count"
     (let* ((file (obsidian-expand-file-name "2.md"))
            (links (ht-get (ht-get obsidian-vault-cache file) 'links)))
       (expect (length (ht-keys links)) :to-equal 11))))

(describe "obsidian unique links with wiki links disabled"
   (before-all (progn
                 (setq markdown-enable-wiki-links nil)
                 (obsidian-change-vault obsidian--test-dir)))
   (after-all (progn
                (setq markdown-enable-wiki-links obsidian--test--original-enable-wiki-links)
                (obsidian-change-vault obsidian--test--original-dir)))

   (it "1.md unique link count"
     (let* ((file (obsidian-expand-file-name "1.md"))
            (links (ht-get (ht-get obsidian-vault-cache file) 'links)))
       (expect (length (ht-keys links)) :to-equal 3)))

   (it "subdir/1.md unique link count"
     (let* ((file (obsidian-expand-file-name "subdir/1.md"))
            (links (ht-get (ht-get obsidian-vault-cache file) 'links)))
       (expect (length (ht-keys links)) :to-equal 0)))

   (it "2.md unique link count"
     (let* ((file (obsidian-expand-file-name "2.md"))
            (links (ht-get (ht-get obsidian-vault-cache file) 'links)))
       (expect (length (ht-keys links)) :to-equal 6))))

(describe "obsidian-file-backlinks"
  (before-all (obsidian-change-vault obsidian--test-dir))
  (after-all (obsidian-change-vault obsidian--test--original-dir))

  (it "1.md using obsidian-file-backlinks"
    (let* ((linkmap (obsidian-file-backlinks "1.md"))
           (file1 (car (hash-table-keys linkmap))))
      (expect (length (hash-table-keys linkmap)) :to-equal 1)
      (expect (file-name-nondirectory file1) :to-equal "2.md")))

  (it "1.md using obsidian-backlinks"
    (let* ((linkmap (obsidian-backlinks "1.md"))
           (file1 (car (hash-table-keys linkmap))))
      (expect (length (hash-table-keys linkmap)) :to-equal 1)
      (expect (file-name-nondirectory file1) :to-equal "2.md"))))

(describe "obsidian-move-file"
  (before-all (obsidian-change-vault obsidian--test-dir))
  (after-all (obsidian-change-vault obsidian--test--original-dir))

  (let* ((orig-file-name
          (expand-file-name (s-concat obsidian--test-dir "/subdir/aliases.md")))
         (moved-file-name
          (expand-file-name (s-concat obsidian--test-dir "/inbox/aliases.md"))))

    (it "obsidian-vault-cache is updated when a file is moved"
        ;; Open file and confirm that it is in the files cache
        (let* ((executing-kbd-macro t)
               (unread-command-events (listify-key-sequence "subdir/aliases.md\n")))
          (call-interactively #'obsidian-jump))
        (expect (obsidian-cached-file-p orig-file-name)  :to-equal t)
        (expect (obsidian-cached-file-p moved-file-name) :to-equal nil)

        ;; Move the file and confirm that new path is in cache and old path is not
        (let* ((make-backup-files nil)
               (executing-kbd-macro t)
               (unread-command-events (listify-key-sequence "inbox\n")))
          (call-interactively #'obsidian-move-file))
        (expect (obsidian-cached-file-p orig-file-name)  :to-equal nil)
        (expect (obsidian-cached-file-p moved-file-name) :to-equal t)

        ;; Return file and confirm that the cache was again updated
        (let* ((make-backup-files nil)
               (executing-kbd-macro t)
               (unread-command-events (listify-key-sequence "subdir\n")))
          (call-interactively #'obsidian-move-file))
        (expect (obsidian-cached-file-p orig-file-name)  :to-equal t)
        (expect (obsidian-cached-file-p moved-file-name) :to-equal nil))))

(describe
 "Insert links for files that don't exist"
 (before-all (progn
               (setq old-inbox obsidian-inbox-directory)
               (setq obsidian-inbox-directory "inbox")
               (obsidian-change-vault obsidian--test-dir)))
 (after-each (obsidian-test--delete-all-test-files))
 (after-all (progn
              (setq obsidian-inbox-directory old-inbox)
              (obsidian-change-vault obsidian--test--original-dir)))

  (it "insert link from vault root when inbox setting is t"
     (obsidian-test--jump-to-file "1.md")
     (newline)
     (let* ((obsidian-create-unfound-files-in-inbox t)
            (executing-kbd-macro t)
            (unread-command-events (listify-key-sequence "bar\n"))
            (bad-path
             (concat obsidian-directory "/bar.md"))
            (good-path
             (concat obsidian-directory "/" obsidian-inbox-directory "/bar.md")))
       (call-interactively #'obsidian-insert-link)
       (expect (file-exists-p bad-path) :to-equal nil)
       (expect (file-exists-p good-path) :to-equal t))
     (kill-whole-line))

 (it "insert link from subdir when inbox setting is t"
     (obsidian-test--jump-to-file "subdir/2-sub.md")
     (newline)
     (let* ((obsidian-create-unfound-files-in-inbox t)
            (executing-kbd-macro t)
            (unread-command-events (listify-key-sequence "bar\n"))
            (bad-path
             (concat obsidian-directory "/subdir/bar.md"))
            (good-path
             (concat obsidian-directory "/" obsidian-inbox-directory "/bar.md")))
       (call-interactively #'obsidian-insert-link)
       (expect (file-exists-p bad-path) :to-equal nil)
       (expect (file-exists-p good-path) :to-equal t))
     (kill-whole-line))

  (it "insert link from vault root when inbox setting is nil"
     (obsidian-test--jump-to-file "1.md")
     (newline)
     (let* ((obsidian-create-unfound-files-in-inbox nil)
            (executing-kbd-macro t)
            (unread-command-events (listify-key-sequence "bar\n"))
            (good-path
             (concat obsidian-directory "/bar.md"))
            (bad-path
             (concat obsidian-directory "/" obsidian-inbox-directory "/bar.md")))
       (call-interactively #'obsidian-insert-link)
       (expect (file-exists-p good-path) :to-equal t)
       (expect (file-exists-p bad-path) :to-equal nil))
     (kill-whole-line))

 (it "insert link from subdir when inbox setting is nil"
     (obsidian-test--jump-to-file "subdir/2-sub.md")
     (newline)
     (let* ((obsidian-create-unfound-files-in-inbox nil)
            (executing-kbd-macro t)
            (unread-command-events (listify-key-sequence "bar\n"))
            (good-path
             (concat obsidian-directory "/subdir/bar.md"))
            (bad-path
             (concat obsidian-directory "/" obsidian-inbox-directory "/bar.md")))
       (call-interactively #'obsidian-insert-link)
       (expect (file-exists-p good-path) :to-equal t)
       (expect (file-exists-p bad-path) :to-equal nil))
     (kill-whole-line)))

(provide 'test-obsidian)
