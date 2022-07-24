(require 'obsidian)
(require 'buttercup)

(obsidian-specify-path "./test_vault")

(describe "-comment macro"
  (it "-comment macro expands to nil"
    (expect (-comment (+ 1 1)) :to-equal nil)))

(describe "check path setting"
  (before-all (obsidian-specify-path "./test_vault"))
  (after-all (obsidian-specify-path "./test_vault"))

  (it "set to current"
    (expect obsidian-directory :to-equal (expand-file-name "./test_vault"))
    (expect (obsidian-specify-path ".") :to-equal (expand-file-name "."))))

(describe "obsidian-descendant-of-p"
  (it "include files right in vault"
    (expect (obsidian-descendant-of-p "./test_vault/1.md" obsidian-directory) :to-be t))
  (it "also include files in trash"
    (expect (obsidian-descendant-of-p "./test_vault/.trash/trash.md" obsidian-directory) :to-be t)))

(describe "obsidian-file?"
  (it "include files right in vault"
    (expect (obsidian-file? "./test_vault/1.md") :to-be t))
  (it "include files in subdirs"
    (expect (obsidian-file? "./test_vault/subdir/1-sub.md") :to-be t))
  (it "exclude files in trash"
    (expect (obsidian-file? "./test_vault/.trash/trash.md") :to-be nil)))

(describe "obsidian-list-all-files"
  (it "check file count"
    (expect (length (obsidian-list-all-files)) :to-equal 5)))

(describe "obsidian-find-tags"
  (it "find tags in string"
    (expect (length (obsidian-find-tags "#foo bar #spam #bar-spam #spam_bar #foo+spam #foo=bar not tags")) :to-equal 6)))

(describe "obsidian-list-all-tags"
  (it "find all tags in the vault"
    (expect (length (obsidian-list-all-tags)) :to-equal 5)))
