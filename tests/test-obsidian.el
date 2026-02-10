;;; test-obsidian.el --- Obsidian Tests -*- coding: utf-8; lexical-binding: t; -*-
(require 'obsidian)
(require 'buttercup)

(defvar obsidian--test-dir (expand-file-name "./tests/test_vault/"))
(defvar obsidian--test--original-dir obsidian--test-dir)
(defvar obsidian--test--original-wiki-link-alias-first obsidian-wiki-link-alias-first)
(defvar obsidian--test--original-enable-wiki-links markdown-enable-wiki-links)
(defvar obsidian--test-number-of-tags 9)
(defvar obsidian--test-number-of-visible-tags 6)
(defvar obsidian--test-number-of-notes 12)
(defvar obsidian--test-number-of-visible-notes 10)
(defvar obsidian--test-number-of-visible-directories 2)
(defvar obsidian--test-visibility-cfg obsidian-include-hidden-files)

(defun obsidian-test--delete-all-test-files ()
  "Function to delete all files potentially left behind by tests."
  (delete-file (concat obsidian--test-dir "/foo.md"))
  (delete-file (concat obsidian--test-dir "/bar.md"))
  (delete-file (concat obsidian--test-dir "/inbox/foo.md"))
  (delete-file (concat obsidian--test-dir "/inbox/bar.md"))
  (delete-file (concat obsidian--test-dir "/subdir/foo.md"))
  (delete-file (concat obsidian--test-dir "/subdir/bar.md")))

(defun obsidian-test--jump-to-file (file)
  "FILE is a path relative to the obsidian vault."
  (let ((obsidian-inbox-directory "inbox")
        (executing-kbd-macro t)
        (unread-command-events (listify-key-sequence (format "%s\n" file))))
    (call-interactively #'obsidian-jump)))

(defun obsidian-test--backlinks-count (file vault)
  "Return the number of backlinks for FILE."
  (let ((bmap (obsidian-backlinks file vault)))
    (seq-reduce #'+ (mapcar #'length (ht-values bmap)) 0)))

(defun obsidian-test--cached-file-p (file vault)
  "Return t if FILE exists in vault cache."
  (if (ht-get (obsidian--vault-cache vault) file) t))

(defmacro obsidian-test--in-test-vault (&rest body)
  "Run BODY in obsidian test vault;
set obsidian--vault-alist to nil;
set var `vault' to test vault"
  `(let ((default-directory ,obsidian--test-dir)
         (obsidian--vault-alist nil)
         (vault ,obsidian--test-dir))
     ,@body))

(defmacro obsidian-test--in-test-vault-with-cache (&rest body)
  "Run BODY in obsidian test vault;
set obsidian--vault-alist to nil;
set var `vault' to test vault;
then rescans the cache"
  `(let ((default-directory ,obsidian--test-dir)
         (obsidian--vault-alist nil)
         (vault ,obsidian--test-dir))
     (obsidian-rescan-cache vault)
     ,@body))

(defmacro obsidian-test--in-temp-vault (&rest body)
  "Run BODY in temporary obsidian dir;
set obsidian--vault-alist to nil;
set var `vault' to test vault"
  `(let* ((tmp-dir (make-temp-file "obs" 'directory))
          (default-directory tmp-dir)
          (obsidian--vault-alist nil)
          (vault tmp-dir))
     (make-directory (expand-file-name ".obsidian" tmp-dir))
     ,@body))


(describe "Per-vault data store"
          (it "is lazily initialized"
              (obsidian-test--in-test-vault
               (expect (obsidian--get-vault-data vault) :to-equal
                       '(:cache nil :aliases nil))
               (expect (length obsidian--vault-alist) :to-be 1)
               (expect (caar obsidian--vault-alist) :to-equal default-directory)))
          (it "with lazy cache init"
              (obsidian-test--in-test-vault
               (expect (hash-table-p (obsidian--vault-cache vault)) :to-be t)
               (expect (length obsidian--vault-alist) :to-be 1)
               (expect (caar obsidian--vault-alist) :to-equal default-directory)))
          (it "with lazy aliases init"
              (obsidian-test--in-test-vault
               (expect (hash-table-p (obsidian--vault-aliases vault)) :to-be t)
               (expect (length obsidian--vault-alist) :to-be 1)
               (expect (caar obsidian--vault-alist) :to-equal default-directory)))
          (it "can be re-initialized"
              (obsidian-test--in-test-vault
               (let ((_ (obsidian--vault-cache vault))
                     (_ (obsidian--vault-aliases vault)))
                 (obsidian--init-vault-data vault)
                 (expect (length obsidian--vault-alist) :to-be 1)
                 (expect obsidian--vault-alist
                         :to-equal
                         `((,vault
                            :cache nil :aliases nil))))))
          (it "will not allow initializing with vault=nil"
              (let (obsidian--vault-alist)
                (expect (obsidian--init-vault-data nil) :to-throw))))

(describe "check vault location"
          (it "is correctly detected"
              (let ((default-directory obsidian--test-dir))
                (expect (obsidian-vault) :to-equal (expand-file-name default-directory))))
          (it "in another location"
              (obsidian-test--in-temp-vault
               (expect (f-same? (obsidian-vault) vault) :to-be t)))
          (it "from a subfolder"
              (let ((default-directory (f-join obsidian--test-dir "subdir")))
                (expect (obsidian-vault) :to-equal (expand-file-name
                                                    obsidian--test-dir))))
          (it "is nil outside of vault"
              (let* ((tmp-dir (make-temp-file "obs" t))
                     (default-directory tmp-dir))
                (expect (obsidian-vault) :to-be nil)
                (expect (obsidian--vault-cache) :to-throw)))
          (it "is detected for an obsidian project within a git project"
              (let* ((tmp-dir (make-temp-file "obs" t))
                     (inner (expand-file-name "obs-vault" tmp-dir))
                     (default-directory inner))
                (make-directory (expand-file-name ".git" tmp-dir))
                (make-directory inner)
                (make-directory (expand-file-name ".obsidian" inner))
                (expect (car (project-current)) :to-be 'obsidian)
                (expect (f-same-p (obsidian-vault) inner) :to-be t)))
          (describe "is detected for a VC project"
                    (it "with a .obsidian marker"
                        (let* ((git-dir (make-temp-file "obs" t))
                               (default-directory git-dir))
                          (make-directory (expand-file-name ".git" git-dir))
                          (make-directory (expand-file-name ".obsidian" git-dir))
                          (expect (car (project-current)) :to-be 'obsidian)
                          (expect (f-same-p (obsidian-vault) git-dir) :to-be t)))
                    (it "inside an obsidian project"
                        (let* ((tmp-dir (make-temp-file "obs" t))
                               (inner (expand-file-name "git-proj" tmp-dir))
                               (default-directory inner))
                          (make-directory (expand-file-name ".obsidian" tmp-dir))
                          (make-directory inner)
                          (make-directory (expand-file-name ".git" inner))
                          (expect (car (project-current)) :to-be 'obsidian)
                          (expect (f-same-p (obsidian-vault) tmp-dir) :to-be t)))
                    (it "regardless of order of project-find-functions"
                        (let* ((tmp-dir (make-temp-file "obs" t))
                               (inner (expand-file-name "git-proj" tmp-dir))
                               (default-directory inner)
                               (orig-hooks project-find-functions))
                          (setq project-find-functions (list 'project-try-vc))
                          (make-directory (expand-file-name ".obsidian" tmp-dir))
                          (make-directory inner)
                          (make-directory (expand-file-name ".git" inner))
                          (expect (car (project-current)) :to-be 'vc)
                          (expect (f-same-p (obsidian-vault) tmp-dir) :to-be t)
                          (setq project-find-functions orig-hooks)))
                    (it "even when the project is not an obsidian project"
                        (let* ((git-dir (make-temp-file "obs" t))
                               (default-directory git-dir)
                               (old-hook project-find-functions))
                          (make-directory (expand-file-name ".git" git-dir))
                          (make-directory (expand-file-name ".obsidian" git-dir))
                          (remove-hook 'project-find-functions
                                       'obsidian-try-project
                                       nil)
                          (expect (car (project-current)) :not :to-be 'obsidian)
                          (expect (f-same-p (obsidian-vault) git-dir) :to-be t)
                          (setq project-find-functions old-hook)))
                    (it "but not without a .obsidian marker"
                        (let* ((git-dir (make-temp-file "obs" t))
                               (default-directory git-dir))
                          (make-directory (expand-file-name ".git" git-dir))
                          (expect (obsidian-vault) :to-be nil)))))

(describe "check ignore directories function"
          (it "obsidian-not-in-excluded-directory-p with a list of directories"
              (let ((file (f-join obsidian--test-dir "inbox/2022-07-24.md"))
                    obsidian-excluded-directories)
                (expect (file-exists-p file) :to-equal t)
                (expect (obsidian-not-in-excluded-directory-p file) :to-be t)
                (let (( obsidian-excluded-directories
                        (cons (concat obsidian--test-dir "/subdir") obsidian-excluded-directories)))
                  (expect (obsidian-not-in-excluded-directory-p file) :to-be t))
                (let (( obsidian-excluded-directories
                        (cons (f-join obsidian--test-dir "inbox") obsidian-excluded-directories)))
                  (expect (obsidian-not-in-excluded-directory-p file) :to-be nil)))))

(describe "check obsidian-file-p with ignored directories"
          (it "inbox file(s) are not in vault cache"
              (obsidian-test--in-test-vault-with-cache
               (let ((obsidian-excluded-directories
                      (list (f-join obsidian--test-dir "inbox")))
                     obsidian-include-hidden-files)
                 (setq obsidian-excluded-directories
                       (list (f-join obsidian--test-dir "inbox")))
                 (obsidian-rescan-cache vault)

                 (expect (length (ht-keys (obsidian--vault-cache vault)))
                         :not :to-equal obsidian--test-number-of-visible-notes)
                 (expect (length (ht-keys (obsidian--vault-cache vault)))
                         :to-equal (1- obsidian--test-number-of-visible-notes))
                 (expect (length (obsidian--directories vault)) :not :to-equal 2)
                 (expect (length (obsidian--directories vault)) :to-equal 1)))))

(describe "obsidian-file-p"
          (it "include files right in vault"
              (obsidian-test--in-test-vault
               (expect (obsidian-file-p
                        (f-join obsidian--test-dir "1.md"))
                       :to-be t)))
          (it "include files in subdirs"
              (obsidian-test--in-test-vault
               (expect (obsidian-file-p
                        (f-join obsidian--test-dir "subdir/1-sub.md"))
                       :to-be t)))
          (it "exclude files in trash"
              (let ((default-directory obsidian--test-dir))
                (expect (obsidian-file-p
                         (f-join ".trash/trash.md"))
                        :to-be nil))))

(describe "obsidian list all visible files"
          (before-all (setq obsidian-include-hidden-files nil))
          (after-all (setq obsidian-include-hidden-files obsidian--test-visibility-cfg))
          (it "check visible file count"
              (obsidian-test--in-test-vault-with-cache
               (expect (length (obsidian--files vault)) :to-equal obsidian--test-number-of-visible-notes))))

(describe "obsidian list all files including hidden files"
          (before-all (setq obsidian-include-hidden-files t))
          (after-all (setq obsidian-include-hidden-files obsidian--test-visibility-cfg))
          (it "check all files count"
              (obsidian-test--in-test-vault-with-cache
               (expect (length (obsidian--files vault)) :to-equal obsidian--test-number-of-notes))))

(describe "obsidian-directories"
          (before-all (setq obsidian-include-hidden-files nil))
          (after-all (setq obsidian-include-hidden-files obsidian--test-visibility-cfg))
          (it "check directory count"
              (obsidian-test--in-test-vault
               (expect (length (obsidian--directories vault)) :to-equal
                       obsidian--test-number-of-visible-directories))))

(describe "obsidian-remove-front-matter-front-string"
          (it "Remove front matter from string"
              (expect (obsidian-remove-front-matter-from-string "---\ntags: [foo]\n---\none\ntwo")
                      :to-equal "one\ntwo"))
          (it "Return string when front matter isn't present"
              (expect (obsidian-remove-front-matter-from-string "---\none\ntwo")
                      :to-equal "---\none\ntwo")
              (expect (obsidian-remove-front-matter-from-string "one\ntwo")
                      :to-equal "one\ntwo")))

(describe "obsidian-find-tags-in-string"
          (it "find tags in string"
              (expect (length (obsidian-find-tags-in-string
                               "#foo bar #spam #bar-spam #spam_bar #foo+spam #foo=bar not tags #123 #+invalidtag"))
                      :to-equal 6)
              (expect (obsidian-find-tags-in-string "---\ntags: \n---") :to-equal nil)
              (expect (obsidian-find-tags-in-string "---\ntags: one\n---") :to-equal nil)
              (expect (obsidian-find-tags-in-string "---\ntags: one two three\n---") :to-equal nil)
              (expect (obsidian-find-tags-in-string "---\ntags: one, two, three\n---") :to-equal nil)
              (expect (obsidian-find-tags-in-string "---\ntags: [one two three]\n---") :to-equal nil)
              (expect (obsidian-find-tags-in-string "---\ntags: [one #two three]\n---") :to-equal nil)
              (expect (obsidian-find-tags-in-string "---\ntags: one, #two, three---\n") :to-equal nil)
              (expect (obsidian-find-tags-in-string "---\ntags: [one, two, three]\n---")
                      :to-equal '("one" "two" "three"))
              (expect (obsidian-find-tags-in-string "---\ntags:\n- one\n- two\n- three\n---\n")
                      :to-equal '("one" "two" "three"))))

(describe "obsidian-find-aliases-in-string"
          (it "find aliases in string"
              (expect (obsidian-find-aliases-in-string "---\naliases: \n---")
                      :to-equal nil)
              (expect (obsidian-find-aliases-in-string "---\naliases: [file1]\n---")
                      :to-equal '("file1"))
              (expect (obsidian-find-aliases-in-string "---\naliases: [file1, file2]\n---")
                      :to-equal '("file1" "file2"))
              (expect (obsidian-find-aliases-in-string "---\naliases:\n- file1\n- file2\n---")
                      :to-equal '("file1" "file2"))))

(describe "obsidian-list-visible-tags"
          (before-all (setq obsidian-include-hidden-files nil))
          (after-all (setq obsidian-include-hidden-files obsidian--test-visibility-cfg))

          (it "find all tags in the vault"
              (obsidian-test--in-test-vault-with-cache
               (expect (length (obsidian--tags vault)) :to-equal obsidian--test-number-of-visible-tags))))

(describe "obsidian list all tags including hidden tags"
          (before-all (setq obsidian-include-hidden-files t))
          (after-all (setq obsidian-include-hidden-files obsidian--test-visibility-cfg))
          (it "find all tags in the vault"
              (obsidian-test--in-test-vault-with-cache
               (expect (length (obsidian--tags vault)) :to-equal obsidian--test-number-of-tags))))

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
          (it "check that front-matter is found"
              (expect (->> obsidian--test-correct-front-matter
                           obsidian-find-yaml-front-matter-in-string
                           (gethash 'aliases)) :to-equal ["AI" "Artificial Intelligence"]))

          (it "check that front-matter is ignored if not at the top of file"
              (expect (obsidian-find-yaml-front-matter-in-string
                       obsidian--test-incorrect-front-matter--not-start-of-file) :to-equal nil))

          (it "check that front-matter in vault is correct"
              (obsidian-test--in-test-vault-with-cache
               (let ((alias-list (obsidian-aliases vault)))
                 (expect (length alias-list) :to-equal 6)
                 (expect (seq-contains-p alias-list "2") :to-equal t)
                 (expect (seq-contains-p alias-list "2-sub-alias") :to-equal t)
                 (expect (seq-contains-p alias-list "complex file name") :to-equal t)
                 (expect (seq-contains-p alias-list "alias-one-off") :to-equal t)
                 (expect (seq-contains-p alias-list "alias1") :to-equal t)
                 (expect (seq-contains-p alias-list "alias2") :to-equal t)))))

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

(describe "obsidian links count including wiki links"
          (before-all (progn
                        (setq markdown-enable-wiki-links t)
                        (setq obsidian-wiki-link-alias-first nil)))
          (after-all (progn
                       (setq markdown-enable-wiki-links
                             obsidian--test--original-enable-wiki-links)
                       (setq obsidian-wiki-link-alias-first
                             obsidian--test--original-wik-link-alias-first)))

          (it "1.md link count"
              (obsidian-test--in-test-vault-with-cache
               (let* ((file (expand-file-name "1.md" vault))
                      (links (ht-get (ht-get (obsidian--vault-cache vault) file) 'links)))
                 (expect (length (ht-keys links)) :to-equal 3))))

          (it "subdir/1-sub.md link count"
              (obsidian-test--in-test-vault-with-cache
               (let* ((file (expand-file-name "subdir/1-sub.md" vault))
                      (links (ht-get (ht-get (obsidian--vault-cache vault) file) 'links)))
                 (expect (length (ht-keys links)) :to-equal 2))))

          (it "2.md link count"
              (obsidian-test--in-test-vault-with-cache
               (let* ((file (expand-file-name "2.md" vault))
                      (links (ht-get (ht-get (obsidian--vault-cache vault) file) 'links))
                      (count (seq-reduce #'+ (ht-map (lambda (k v) (length v)) links) 0)))
                 (expect count :to-equal 8))))

          (it "2-vault-paths.md link count"
              (obsidian-test--in-test-vault-with-cache
               (let* ((file (expand-file-name "2-vault-paths.md" vault))
                      (links (ht-get (ht-get (obsidian--vault-cache vault) file) 'links))
                      (count (seq-reduce #'+ (ht-map (lambda (k v) (length v)) links) 0)))
                 (expect count :to-equal 9)))))

(describe "obsidian links with wiki links disabled"
          (before-all (setq markdown-enable-wiki-links nil))
          (after-all (setq markdown-enable-wiki-links
                           obsidian--test--original-enable-wiki-links))

          (it "1.md link count"
              (obsidian-test--in-test-vault-with-cache
               (let* ((file (expand-file-name "1.md" vault))
                      (links (ht-get (ht-get (obsidian--vault-cache vault) file) 'links)))
                 (expect (length (ht-keys links)) :to-equal 3))))

          (it "subdir/1-sub.md link count"
              (obsidian-test--in-test-vault-with-cache
               (let* ((file (expand-file-name "subdir/1-sub.md" vault))
                      (links (ht-get (ht-get (obsidian--vault-cache vault) file) 'links)))
                 (expect (length (ht-keys links)) :to-equal 1))))

          (it "2.md link count"
              (obsidian-test--in-test-vault-with-cache
               (let* ((file (expand-file-name "2.md" vault))
                      (links (ht-get (ht-get (obsidian--vault-cache vault) file) 'links))
                      (count (seq-reduce #'+ (ht-map (lambda (k v) (length v)) links) 0)))
                 (expect count :to-equal 4))))

          (it "2-vault-paths.md link count"
              (obsidian-test--in-test-vault-with-cache
               (let* ((file (expand-file-name "2-vault-paths.md" vault))
                      (links (ht-get (ht-get (obsidian--vault-cache vault) file) 'links))
                      (count (seq-reduce #'+ (ht-map (lambda (k v) (length v)) links) 0)))
                 (expect count :to-equal 5)))))

(describe "obsidian-backlinks with wiki links"
          (before-all (setq markdown-enable-wiki-links t))
          (after-all (setq markdown-enable-wiki-links
                           obsidian--test--original-enable-wiki-links))

          (it "1.md using obsidian-backlinks"
              (obsidian-test--in-test-vault-with-cache
               (let* ((file (obsidian-file-to-absolute-path "1.md" vault))
                      (count (obsidian-test--backlinks-count file vault)))
                 (expect count :to-equal 3))))

          (it "2-sub with spaces and буквы.md using obsidian-backlinks"
              (obsidian-test--in-test-vault-with-cache
               (let* ((file (obsidian-file-to-absolute-path "2-sub with spaces and буквы.md" vault))
                      (count (obsidian-test--backlinks-count file vault)))
                 (expect count :to-equal 8)))))

(describe "obsidian-backlinks without wiki links"
          (before-all (setq markdown-enable-wiki-links nil))
          (after-all (setq markdown-enable-wiki-links
                           obsidian--test--original-enable-wiki-links))

          (it "1.md using obsidian-backlinks"
              (obsidian-test--in-test-vault-with-cache
               (let* ((file (obsidian-file-to-absolute-path "1.md" vault))
                      (count (obsidian-test--backlinks-count file vault)))
                 (expect count :to-equal 2))))

          (it "2-sub with spaces and буквы.md using obsidian-backlinks"
              (obsidian-test--in-test-vault-with-cache
               (let* ((file (obsidian-file-to-absolute-path "2-sub with spaces and буквы.md" vault))
                      (count (obsidian-test--backlinks-count file vault)))
                 (expect count :to-equal 3)))))

(describe "following a link"
          (it "makes you end up in the target file"
              (obsidian-test--in-test-vault-with-cache
               (save-excursion
                 (find-file "1.md")
                 (goto-char (point-min))
                 (search-forward "[2-sub]")
                 (obsidian-follow-markdown-link-at-point)
                 (expect (file-name-nondirectory buffer-file-name) :to-equal "2-sub.md"))))
          (it "adds the original buffer to the jump list"
              (obsidian-test--in-test-vault-with-cache
               (let ((obsidian--jump-list nil))
                 (save-excursion
                   (find-file "1.md")
                   (goto-char (point-min))
                   (search-forward "[2-sub]")
                   (obsidian-follow-markdown-link-at-point)
                   (expect (length obsidian--jump-list) :to-be 1)
                   (expect (buffer-file-name (marker-buffer
                                             (car obsidian--jump-list))) :to-equal
                           (obsidian-file-to-absolute-path "1.md" vault)))))))

(describe "obsidian-jump"
          (it "opens a file in buffer"
              (obsidian-test--in-test-vault-with-cache
               (let ((executing-kbd-macro t)
                     (unread-command-events (listify-key-sequence "subdir/aliases.md\n")))
                 (call-interactively #'obsidian-jump))
               (expect (buffer-file-name) :to-equal (f-join obsidian--test-dir "subdir/aliases.md")))))

(describe "obsidian-move-file"
          (let ((orig-file-name
                 (expand-file-name (f-join obsidian--test-dir "subdir/aliases.md")))
                (moved-file-name
                 (expand-file-name (f-join obsidian--test-dir "inbox/aliases.md"))))

            (it "(obsidian--vault-cache) is updated when a file is moved"
                ;; Open file and confirm that it is in the files cache
                (obsidian-test--in-test-vault-with-cache
                 (let ((executing-kbd-macro t)
                       (unread-command-events (listify-key-sequence "subdir/aliases.md\n")))
                   (call-interactively #'obsidian-jump))
                 (expect (obsidian-test--cached-file-p orig-file-name vault)  :to-equal t)
                 (expect (obsidian-test--cached-file-p moved-file-name vault) :to-equal nil)

                 ;; Move the file and confirm that new path is in cache and old path is not
                 (let ((make-backup-files nil)
                       (executing-kbd-macro t)
                       (unread-command-events (listify-key-sequence "inbox\n")))
                   (call-interactively #'obsidian-move-file))
                 (expect (obsidian-test--cached-file-p orig-file-name vault)  :to-equal nil)
                 (expect (obsidian-test--cached-file-p moved-file-name vault) :to-equal t)

                 ;; Return file and confirm that the cache was again updated
                 (let ((make-backup-files nil)
                       (executing-kbd-macro t)
                       (unread-command-events (listify-key-sequence "subdir\n")))
                   (call-interactively #'obsidian-move-file))
                 (expect (obsidian-test--cached-file-p orig-file-name vault)  :to-equal t)
                 (expect (obsidian-test--cached-file-p moved-file-name vault) :to-equal nil)))))

(describe
 "Insert links for files that don't exist"
 (after-each (obsidian-test--delete-all-test-files))

 (it "insert link from vault root when inbox setting is t"
     (obsidian-test--in-test-vault-with-cache
      (obsidian-test--jump-to-file "1.md")
      (newline)
      (let ((orig-inbox obsidian-inbox-directory))
        (setq obsidian-inbox-directory "inbox")
        (let ((obsidian-create-unfound-files-in-inbox t)
              (executing-kbd-macro t)
              (unread-command-events (listify-key-sequence "bar\n"))
              (bad-path
               (f-join obsidian--test-dir "bar.md"))
              (good-path
               (f-join obsidian--test-dir obsidian-inbox-directory "bar.md")))
          (call-interactively #'obsidian-insert-link)
          (expect (file-exists-p bad-path) :to-equal nil)
          (expect (file-exists-p good-path) :to-equal t))
        (setq obsidian-inbox-directory orig-inbox))
      (kill-whole-line)))

 (it "insert link from subdir when inbox setting is t"
     (obsidian-test--in-test-vault-with-cache
      (obsidian-test--jump-to-file "subdir/2-sub.md")
      (newline)
      (let ((orig-inbox obsidian-inbox-directory))
        (setq obsidian-inbox-directory "inbox")
        (let ((obsidian-create-unfound-files-in-inbox t)
              (executing-kbd-macro t)
              (unread-command-events (listify-key-sequence "bar\n"))
              (bad-path
               (f-join obsidian--test-dir "subdir/bar.md"))
              (good-path
               (f-join obsidian--test-dir obsidian-inbox-directory "bar.md"))
              (bad-path-root
               (concat obsidian--test-dir "/bar.md")))
          (call-interactively #'obsidian-insert-link)
          (expect (file-exists-p bad-path) :to-equal nil)
          (expect (file-exists-p good-path) :to-equal t)
          (expect (file-exists-p bad-path-root) :to-equal nil))
        (setq obsidian-inbox-directory orig-inbox))
      (kill-whole-line)))

 (it "insert link from vault root when inbox setting is nil"
     (obsidian-test--in-test-vault-with-cache
      (obsidian-test--jump-to-file "1.md")
      (newline)
      (let ((orig-inbox obsidian-inbox-directory))
        (setq obsidian-inbox-directory "inbox")
        (let ((obsidian-create-unfound-files-in-inbox nil)
              (executing-kbd-macro t)
              (unread-command-events (listify-key-sequence "bar\n"))
              (good-path
               (f-join obsidian--test-dir "bar.md"))
              (bad-path
               (f-join obsidian--test-dir obsidian-inbox-directory "bar.md")))
          (call-interactively #'obsidian-insert-link)
          (expect (file-exists-p good-path) :to-equal t)
          (expect (file-exists-p bad-path) :to-equal nil))
        (setq obsidian-inbox-directory orig-inbox))
      (kill-whole-line)))

 (it "insert link from subdir when inbox setting is nil"
     (obsidian-test--in-test-vault-with-cache
      (obsidian-test--jump-to-file "subdir/2-sub.md")
      (newline)
      (let ((orig-inbox obsidian-inbox-directory))
        (setq obsidian-inbox-directory "inbox")
        (let* ((obsidian-create-unfound-files-in-inbox nil)
               (executing-kbd-macro t)
               (unread-command-events (listify-key-sequence "bar\n"))
               (good-path
                (f-join obsidian--test-dir "subdir/bar.md"))
               (bad-path
                (f-join obsidian--test-dir obsidian-inbox-directory "bar.md"))
               (bad-path-root
                (f-join obsidian--test-dir "bar.md")))
          (call-interactively #'obsidian-insert-link)
          (expect (file-exists-p good-path) :to-equal t)
          (expect (file-exists-p bad-path) :to-equal nil)
          (expect (file-exists-p bad-path-root) :to-equal nil))
        (setq obsidian-inbox-directory orig-inbox))
      (kill-whole-line))))

(provide 'test-obsidian)
