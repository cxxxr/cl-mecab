(defpackage #:mecab.util
  (:use #:cl)
  (:export #:split-string))

(in-package #:mecab.util)

(defun split-string (separator string &key (start 0) end)
  (labels ((f (start acc)
             (let ((pos (position separator string :start start :end end)))
               (if (null pos)
                 (nreverse (cons (subseq string start end) acc))
                 (f (1+ pos) (cons (subseq string start pos) acc))))))
    (f start '())))
