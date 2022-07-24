(require 'obsidian)
(require 'buttercup)

(obsidian-specify-path "./test_vault")

(describe "-comment macro"
  (it "-comment macro expands to nil"
    (expect (-comment (+ 1 1)) :to-equal nil)))
