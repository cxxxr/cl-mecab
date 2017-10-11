(defpackage #:mecab
  (:use
   #:cl
   #:mecab.low-level
   #:mecab.util)
  (:export
   #:make-mecab
   #:with-mecab
   #:parse
   #:parse-nbest))

(in-package #:mecab)

(defvar *mecab*)

(defun make-mecab (&optional (option ""))
  (mecab-new2 option))

(defmacro with-mecab ((&optional (option "")) &body body)
  `(let ((*mecab* (make-mecab ,option)))
     (unwind-protect (progn ,@body)
       (mecab-destroy *mecab*))))

(defun %split-output (input-stream)
  (loop :for line := (read-line input-stream nil)
        :while line
        :for tabpos := (position #\Tab line)
        :while tabpos
        :collect (cons (subseq line 0 tabpos)
                       (split-string #\, line :start (1+ tabpos)))))

(defun split-output (output)
  (with-input-from-string (input-stream output)
    (%split-output input-stream)))

(defun split-output-n (n output)
  (with-input-from-string (input-stream output)
    (loop :repeat n :collect (%split-output input-stream))))

(defun parse (text)
  (split-output (mecab-sparse-tostr *mecab* text)))

(defun parse-nbest (n text)
  (split-output-n n (mecab-nbest-sparse-tostr *mecab* n text)))
