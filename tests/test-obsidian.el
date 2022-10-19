(require 'obsidian)
(require 'buttercup)

(defvar obsidian--test-dir "./tests/test_vault")
(defvar obsidian--test--original-dir (or obsidian-directory obsidian--test-dir))
(defvar obsidian--test--original-tags-list obsidian--tags-list)
(defvar obsidian--test-number-of-tags 6)
(defvar obsidian--test-number-of-notes 9)

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

(describe "obsidian-file-p"
  (before-all (obsidian-specify-path obsidian--test-dir))
  (after-all (obsidian-specify-path obsidian--test--original-dir))

  (it "include files right in vault"
    (expect (obsidian-file-p "./tests/test_vault/1.md") :to-be t))
  (it "include files in subdirs"
    (expect (obsidian-file-p "./tests/test_vault/subdir/1-sub.md") :to-be t))
  (it "exclude files in trash"
    (expect (obsidian-file-p "./tests/test_vault/.trash/trash.md") :to-be nil)))

(describe "obsidian-list-all-files"
  (before-all (obsidian-specify-path obsidian--test-dir))
  (after-all (obsidian-specify-path obsidian--test--original-dir))

  (it "check file count"
    (expect (length (obsidian-list-all-files)) :to-equal obsidian--test-number-of-notes)))

(describe "obsidian-find-tags"
  (before-all (obsidian-specify-path obsidian--test-dir))
  (after-all (obsidian-specify-path obsidian--test--original-dir))

  (it "find tags in string"
    (expect (length (obsidian-find-tags "#foo bar #spam #bar-spam #spam_bar #foo+spam #foo=bar not tags")) :to-equal 6)))

(describe "obsidian-list-all-tags"
  (before-all (obsidian-specify-path obsidian--test-dir))
  (after-all (obsidian-specify-path obsidian--test--original-dir))

  (it "find all tags in the vault"
    (expect (length (obsidian-list-all-tags)) :to-equal obsidian--test-number-of-tags)))

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
	      (length obsidian--tags-list)) :to-equal obsidian--test-number-of-tags)))


(defvar-local obsidian--test-correct-front-matter "---
aliases: [AI, Artificial Intelligence]
tags: [one, two, three]
key4:
- four
- five
- six
---
")
(defvar obsidian--test-incorret-front-matter--not-start-of-file (s-concat "# Header\n" obsidian--test-correct-front-matter))

(describe "obsidian-aliases"
  (before-all (progn
		(obsidian-specify-path obsidian--test-dir)
		(setq obsidian--tags-list nil)))
  (after-all (progn
	       (obsidian-specify-path obsidian--test--original-dir)
	       (setq obsidian--tags-list obsidian--test--original-tags-list)))

  (it "check that front-matter is found"
    (expect (gethash 'aliases (obsidian-find-yaml-front-matter obsidian--test-correct-front-matter))
	    :to-equal ["AI" "Artificial Intelligence"]))

  (it "check that front-matter is ignored if not at the top of file"
    (expect (obsidian-find-yaml-front-matter obsidian--test-incorret-front-matter--not-start-of-file)
	    :to-equal nil)))

(describe "obsidian--link-p"
  (it "non link"
    (expect (obsidian--link-p "not link") :to-equal nil))

  (it "wiki link"
    (expect (obsidian--link-p "[[foo.md]]") :to-be-truthy)
    (expect (obsidian--link-p "[[foo]]") :to-be-truthy)
    (expect (obsidian--link-p "[[foo|annotated link]]") :to-be-truthy))

  (it "markdown link"
    (expect (obsidian--link-p "[foo](bar)") :to-be-truthy)
    (expect (obsidian--link-p "[foo](bar.md)") :to-be-truthy)))

(describe "obsidian--find-links-to-file"
  (before-all (obsidian-specify-path obsidian--test-dir))
  (after-all (obsidian-specify-path obsidian--test--original-dir))

  (it "1.md"
    (expect (obsidian--find-links-to-file "1.md") :to-equal '("2.md"))))

(describe "extract elgrep context"
  (before-all (obsidian-specify-path obsidian--test-dir))
  (after-all (obsidian-specify-path obsidian--test--original-dir))

  (it "correct extraction"
      (expect
       (obsidian--elgrep-get-context (car (obsidian--grep "subdir/2-sub.md"))) :to-equal "[insert link md](subdir/2-sub.md) text after markdown link")))

(provide 'test-obsidian)
