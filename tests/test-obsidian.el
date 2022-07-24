(require 'obsidian)
(require 'buttercup)

(defvar obsidian--test-dir "./tests/test_vault")
(defvar obsidian--test--original-dir (or obsidian-directory obsidian--test-dir))
(defvar obsidian--test--original-tags-list obsidian--tags-list)

(describe "-comment macro"
  (it "-comment macro expands to nil"
    (expect (-comment (+ 1 1)) :to-equal nil)))

(describe "check path setting"
  (before-all (obsidian-specify-path obsidian--test-dir))
  (after-all (obsidian-specify-path obsidian--test--original-dir))

  (it "set to current"
    (expect obsidian-directory :to-equal (expand-file-name obsidian--test-dir))
    (expect (obsidian-specify-path ".") :to-equal (expand-file-name "."))))

(describe "obsidian-descendant-of-p"
  (before-all (obsidian-specify-path obsidian--test-dir))
  (after-all (obsidian-specify-path obsidian--test--original-dir))

  (it "include files right in vault"
    (expect (obsidian-descendant-of-p "./tests/test_vault/1.md" obsidian-directory) :to-be t))
  (it "also include files in trash"
    (expect (obsidian-descendant-of-p "./tests/test_vault/.trash/trash.md" obsidian-directory) :to-be t)))

(describe "obsidian-file?"
  (before-all (obsidian-specify-path obsidian--test-dir))
  (after-all (obsidian-specify-path obsidian--test--original-dir))

  (it "include files right in vault"
    (expect (obsidian-file? "./tests/test_vault/1.md") :to-be t))
  (it "include files in subdirs"
    (expect (obsidian-file? "./tests/test_vault/subdir/1-sub.md") :to-be t))
  (it "exclude files in trash"
    (expect (obsidian-file? "./tests/test_vault/.trash/trash.md") :to-be nil)))

(describe "obsidian-list-all-files"
  (before-all (obsidian-specify-path obsidian--test-dir))
  (after-all (obsidian-specify-path obsidian--test--original-dir))

  (it "check file count"
    (expect (length (obsidian-list-all-files)) :to-equal 5)))

(describe "obsidian-find-tags"
  (before-all (obsidian-specify-path obsidian--test-dir))
  (after-all (obsidian-specify-path obsidian--test--original-dir))

  (it "find tags in string"
    (expect (length (obsidian-find-tags "#foo bar #spam #bar-spam #spam_bar #foo+spam #foo=bar not tags")) :to-equal 6)))

(describe "obsidian-list-all-tags"
  (before-all (obsidian-specify-path obsidian--test-dir))
  (after-all (obsidian-specify-path obsidian--test--original-dir))

  (it "find all tags in the vault"
    (expect (length (obsidian-list-all-tags)) :to-equal 5)))

(describe "obsidian-update"
  (before-all (progn
		(obsidian-specify-path obsidian--test-dir)
		(setq obsidian--tags-list nil)))
  (after-all (progn
	       (obsidian-specify-path obsidian--test--original-dir)
	       (setq obsidian--tags-list obsidian--test--original-tags-list)))

  (it "check that tags var is empty before update"
    (expect obsidian--tags-list :to-be nil))
  (it "check tags are filled out after update"
    (expect (progn
	      (obsidian-update)
	      (length obsidian--tags-list)) :to-equal 5)))
