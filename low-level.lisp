(defpackage #:mecab.low-level
  (:use #:cl #:cffi)
  (:export #:mecab-new
           #:mecab-new2
           #:mecab-version
           #:mecab-strerror
           #:mecab-destroy
           #:mecab-get-partial
           #:mecab-set-partial
           #:mecab-get-theta
           #:mecab-set-theta
           #:mecab-get-lattice-level
           #:mecab-set-lattice-level
           #:mecab-get-all-morphs
           #:mecab-set-all-morphs
           #:mecab-sparse-tostr
           #:mecab-nbest-sparse-tostr))

(in-package #:mecab.low-level)

(defctype size_t :uint) ;; 32-bit only

(define-foreign-library libmecab
  (:unix "libmecab.so")
  (:darwin "libmecab.dylib")
  (:windows "libmecab.dll"))

(use-foreign-library libmecab)

#|
int           mecab_do(int argc, char **argv);
int           mecab_dict_index(int argc, char **argv);
int           mecab_dict_gen(int argc, char **argv);
int           mecab_cost_train(int argc, char **argv);
int           mecab_system_eval(int argc, char **argv);
int           mecab_test_gen(int argc, char **argv);
|#

(defctype char* :pointer)

(defctype mecab-dictionary-info-t* :pointer)
(defctype mecab-path-t* :pointer)
(defctype mecab-node-t* :pointer)
(defctype mecab-node-t** :pointer)

(defcstruct mecab-dictionary-info-t
  (filename :string)
  (charset :string)
  (size :uint)
  (type :int)
  (lsize :uint)
  (rsize :uint)
  (version :ushort)
  (next mecab-dictionary-info-t*))

(defcstruct mecab-path-t
  (rnode mecab-node-t*)
  (rnext mecab-node-t*)
  (lnode mecab-node-t*)
  (lnext mecab-node-t*)
  (cost :int)
  (prob :float))

(defcstruct mecab-node-t
  (prev mecab-node-t*)
  (next mecab-node-t*)
  (enext mecab-node-t*)
  (bnext mecab-node-t*)
  (rpath mecab-path-t*)
  (lpath mecab-path-t*)
  (surface :string)
  (feature :string)
  (id :uint)
  (length :ushort)
  (rlength :ushort)
  (rcAttr :ushort)
  (lcAttr :ushort)
  (posid :ushort)
  (char-type :uchar)
  (stat :uchar)
  (isbest :uchar)
  (alpha :float)
  (beta :float)
  (prob :float)
  (wcost :short)
  (cost :long))

(defcenum mecab-node-stat
  mecab-nor-node
  mecab-unk-node
  mecab-bos-node
  mecab-eos-node
  mecab-eon-node)

(defcenum mecab-dictionary-info-type
  mecab-sys-dic
  mecab-usr-dic
  mecab-unk-dic)

(defcenum mecab-lattice-request-type
  (mecab-one-best           1)
  (mecab-nbest              2)
  (mecab-partial            4)
  (mecab-marginal-prob      8)
  (mecab-alternative        16)
  (mecab-all-morphs         32)
  (mecab-allocate-sentence  64))

(defcenum mecab-lattice-boundary-constraint-type
  (mecab-any-boundary   0)
  (mecab-token-boundary 1)
  (mecab-inside-token   2))

(defctype mecab-t* :pointer)
(defctype mecab-model-t* :pointer)
(defctype mecab-lattice-t* :pointer)
(defctype mecab-dictionary-info-t* :pointer)
(defctype mecab-node-t* :pointer)
(defctype mecab-path-t* :pointer)

(defcfun mecab-new mecab-t*
  (argc :int)
  (argv :string+ptr))

(defcfun mecab-new2 mecab-t*
  (arg :string))

(defcfun mecab-version :string)

(defcfun mecab-strerror :string
  (mecab mecab-t*))

(defcfun mecab-destroy :void
  (mecab mecab-t*))

(defcfun mecab-get-partial :int
  (mecab mecab-t*))

(defcfun mecab-set-partial :void
  (mecab mecab-t*)
  (partial :int))

(defcfun mecab-get-theta :float
  (mecab mecab-t*))

(defcfun mecab-set-theta :void
  (mecab mecab-t*)
  (theta :float))

(defcfun mecab-get-lattice-level :int
  (mecab mecab-t*))

(defcfun mecab-set-lattice-level :void
  (mecab mecab-t*)
  (level :int))

(defcfun mecab-get-all-morphs :int
  (mecab mecab-t*))

(defcfun mecab-set-all-morphs :void
  (mecab mecab-t*)
  (all-morphs :int))

(defcfun mecab-parse-lattice :int
  (mecab mecab-t*)
  (lattice mecab-lattice-t*))

(defcfun mecab-sparse-tostr :string
  (mecab mecab-t*)
  (str :string))

(defcfun mecab-sparse-tostr2 :string
  (mecab mecab-t*)
  (str :string)
  (len size_t))

(defcfun mecab-sparse-tostr3 char*
  (mecab mecab-t*)
  (str :string)
  (len size_t)
  (ostr char*)
  (olen size_t))

;(defcfun mecab-sparse-tonode )
;(defcfun mecab-sparse-tonode2 )

(defcfun mecab-nbest-sparse-tostr :string
  (mecab mecab-t*)
  (n size_t)
  (str :string))

;(defcfun mecab-nbest-sparse-tostr2 )
;(defcfun mecab-nbest-sparse-tostr3 )

(defcfun mecab-nbest-init :int
  (mecab mecab-t*)
  (str :string))

(defcfun mecab-nbest-init2 :int
  (mecab mecab-t*)
  (str :string)
  (len size_t))

(defcfun mecab-nbest-next-tostr :string
  (mecab mecab-t*))

;(defcfun mecab-nbest-next-tostr2 )

(defcfun mecab-nbest-next-tonode mecab-node-t*
  (mecab mecab-t*))

(defcfun mecab-format-node :string
  (mecab mecab-t*)
  (node mecab-node-t*))

(defcfun mecab-dictionary-info mecab-dictionary-info-t*
  (mecab mecab-t*))

(defcfun mecab-lattice-new mecab-lattice-t*)

(defcfun mecab-lattice-destroy :void
  (lattice mecab-lattice-t*))

(defcfun mecab-lattice-clear :void
  (lattice mecab-lattice-t*))

(defcfun mecab-lattice-is-available :int
  (lattice mecab-lattice-t*))

(defcfun mecab-lattice-get-bos-node mecab-node-t*
  (lattice mecab-lattice-t*))

(defcfun mecab-lattice-get-eos-node mecab-node-t*
  (lattice mecab-lattice-t*))

(defcfun mecab-lattice-get-all-begin-nodes mecab-node-t**
  (lattice mecab-lattice-t*))

(defcfun mecab-lattice-get-all-end-nodes mecab-node-t**
  (lattice mecab-lattice-t*))

;(defcfun mecab-lattice-get-begin-nodes )
;(defcfun mecab-lattice-get-end-nodes )
;(defcfun mecab-lattice-get-sentence )
;(defcfun mecab-lattice-set-sentence )
;(defcfun mecab-lattice-set-sentence2 )
;(defcfun mecab-lattice-get-size )
;(defcfun mecab-lattice-get-z )
;(defcfun mecab-lattice-set-z )
;(defcfun mecab-lattice-get-theta )
;(defcfun mecab-lattice-set-theta )
;(defcfun mecab-lattice-next )
;(defcfun mecab-lattice-get-request-type )
;(defcfun mecab-lattice-has-request-type )
;(defcfun mecab-lattice-set-request-type )
;(defcfun mecab-lattice-add-request-type )
;(defcfun mecab-lattice-remove-request-type )
;(defcfun mecab-lattice-new-node )
;(defcfun mecab-lattice-tostr )
;(defcfun mecab-lattice-tostr2 )
;(defcfun mecab-lattice-nbest-tostr )
;(defcfun mecab-lattice-nbest-tostr2 )
;(defcfun mecab-lattice-has-constraint )
;(defcfun mecab-lattice-get-boundary-constraint )
;(defcfun mecab-lattice-get-feature-constraint )
;(defcfun mecab-lattice-set-boundary-constraint )
;(defcfun mecab-lattice-set-feature-constraint )
;(defcfun mecab-lattice-set-result )
;(defcfun mecab-lattice-strerror )
;(defcfun mecab-model-new )
;(defcfun mecab-model-new2 )
;(defcfun mecab-model-destroy )
;(defcfun mecab-model-new-tagger )
;(defcfun mecab-model-new-lattice )
;(defcfun mecab-model-swap )
;(defcfun mecab-dictionary-info-t )
;(defcfun mecab-model-transition-cost )
;(defcfun mecab-model-lookup )

#|
(loop :for form :in (uiop:read-file-forms "low-level.lisp")
      :when (eq (car form) 'defcfun)
      :collect (cadr form))
|#
