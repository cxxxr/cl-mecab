(asdf:defsystem "mecab"
  :depends-on ("cffi" "uiop")
  :serial t
  :components ((:file "util")
               (:file "low-level")
               (:file "mecab")))
