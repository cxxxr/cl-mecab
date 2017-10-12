(defpackage #:mecab.low-level
  (:use #:cl #:cffi)
  (:export #:size-t #:char* #:char** #:mecab-dictionary-info-t* #:mecab-path-t*
           #:mecab-node-t* #:mecab-node-t** #:mecab-dictionary-info-t #:mecab-path-t
           #:mecab-node-t #:mecab-eon-node #:mecab-eos-node #:mecab-bos-node
           #:mecab-unk-node #:mecab-nor-node #:mecab-node-stat #:mecab-unk-dic
           #:mecab-usr-dic #:mecab-sys-dic #:mecab-dictionary-info-type
           #:mecab-allocate-sentence #:mecab-all-morphs #:mecab-alternative
           #:mecab-marginal-prob #:mecab-partial #:mecab-nbest #:mecab-one-best
           #:mecab-lattice-request-type #:mecab-inside-token #:mecab-token-boundary
           #:mecab-any-boundary #:mecab-lattice-boundary-constraint-type #:mecab-t*
           #:mecab-model-t* #:mecab-lattice-t* #:mecab-dictionary-info-t* #:mecab-node-t*
           #:mecab-path-t* #:mecab-new #:mecab-new2 #:mecab-version #:mecab-strerror
           #:mecab-destroy #:mecab-get-partial #:mecab-set-partial #:mecab-get-theta
           #:mecab-set-theta #:mecab-get-lattice-level #:mecab-set-lattice-level
           #:mecab-get-all-morphs #:mecab-set-all-morphs #:mecab-parse-lattice
           #:mecab-sparse-tostr #:mecab-sparse-tostr2 #:mecab-sparse-tostr3
           #:mecab-sparse-tonode #:mecab-sparse-tonode2 #:mecab-nbest-sparse-tostr
           #:mecab-nbest-sparse-tostr2 #:mecab-nbest-sparse-tostr3 #:mecab-nbest-init
           #:mecab-nbest-init2 #:mecab-nbest-next-tostr #:mecab-nbest-next-tostr2
           #:mecab-nbest-next-tonode #:mecab-format-node #:mecab-dictionary-info
           #:mecab-lattice-new #:mecab-lattice-destroy #:mecab-lattice-clear
           #:mecab-lattice-is-available #:mecab-lattice-get-bos-node
           #:mecab-lattice-get-eos-node #:mecab-lattice-get-all-begin-nodes
           #:mecab-lattice-get-all-end-nodes #:mecab-lattice-get-begin-nodes
           #:mecab-lattice-get-end-nodes #:mecab-lattice-get-sentence
           #:mecab-lattice-set-sentence #:mecab-lattice-set-sentence2
           #:mecab-lattice-get-size #:mecab-lattice-get-z #:mecab-lattice-set-z
           #:mecab-lattice-get-theta #:mecab-lattice-set-theta #:mecab-lattice-next
           #:mecab-lattice-get-request-type #:mecab-lattice-has-request-type
           #:mecab-lattice-set-request-type #:mecab-lattice-add-request-type
           #:mecab-lattice-remove-request-type #:mecab-lattice-new-node
           #:mecab-lattice-tostr #:mecab-lattice-tostr2 #:mecab-lattice-nbest-tostr
           #:mecab-lattice-nbest-tostr2 #:mecab-lattice-has-constraint
           #:mecab-lattice-get-boundary-constraint #:mecab-lattice-get-feature-constraint
           #:mecab-lattice-set-boundary-constraint #:mecab-lattice-set-feature-constraint
           #:mecab-lattice-set-result #:mecab-lattice-strerror #:mecab-model-new
           #:mecab-model-new2 #:mecab-model-destroy #:mecab-model-new-tagger
           #:mecab-model-new-lattice #:mecab-model-swap #:mecab-model-dictionary-info
           #:mecab-model-transition-cost #:mecab-model-lookup #:mecab-do
           #:mecab-dict-index #:mecab-dict-gen #:mecab-cost-train #:mecab-system-eval
           #:mecab-test-gen))

(in-package #:mecab.low-level)

(defctype size-t :uint) ;; 32-bit only

(define-foreign-library libmecab
  (:unix "libmecab.so")
  (:darwin "libmecab.dylib")
  (:windows "libmecab.dll"))

(use-foreign-library libmecab)

(defctype char* :pointer)
(defctype char** :pointer)

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
  (len size-t))

(defcfun mecab-sparse-tostr3 char*
  (mecab mecab-t*)
  (str :string)
  (len size-t)
  (ostr char*)
  (olen size-t))

(defcfun mecab-sparse-tonode mecab-node-t*
  (mecab mecab-t*)
  (str :string))

(defcfun mecab-sparse-tonode2 mecab-node-t*
  (mecab mecab-t*)
  (str :string)
  (len size-t))

(defcfun mecab-nbest-sparse-tostr :string
  (mecab mecab-t*)
  (n size-t)
  (str :string))

(defcfun mecab-nbest-sparse-tostr2 :string
  (mecab mecab-t*)
  (n size-t)
  (str :string)
  (len size-t))

(defcfun mecab-nbest-sparse-tostr3 char*
  (mecab mecab-t*)
  (n size-t)
  (str :string)
  (len size-t)
  (ostr char*)
  (olen size-t))

(defcfun mecab-nbest-init :int
  (mecab mecab-t*)
  (str :string))

(defcfun mecab-nbest-init2 :int
  (mecab mecab-t*)
  (str :string)
  (len size-t))

(defcfun mecab-nbest-next-tostr :string
  (mecab mecab-t*))

(defcfun mecab-nbest-next-tostr2 char*
  (mecab mecab-t*)
  (ostr char*)
  (olen size-t))

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

(defcfun mecab-lattice-get-begin-nodes mecab-node-t*
  (lattice mecab-lattice-t*)
  (pos size-t))

(defcfun mecab-lattice-get-end-nodes mecab-node-t*
  (lattice mecab-lattice-t*)
  (pos size-t))

(defcfun mecab-lattice-get-sentence :string
  (lattice mecab-lattice-t*))

(defcfun mecab-lattice-set-sentence :void
  (lattice mecab-lattice-t*)
  (sentence :string))

(defcfun mecab-lattice-set-sentence2 :void
  (lattice mecab-lattice-t*)
  (sentence :string)
  (len size-t))

(defcfun mecab-lattice-get-size size-t
  (lattice mecab-lattice-t*))

(defcfun mecab-lattice-get-z :double
  (lattice mecab-lattice-t*))

(defcfun mecab-lattice-set-z :void
  (lattice mecab-lattice-t*)
  (z :double))

(defcfun mecab-lattice-get-theta :double
  (lattice mecab-lattice-t*))

(defcfun mecab-lattice-set-theta :void
  (lattice mecab-lattice-t*)
  (theta :double))

(defcfun mecab-lattice-next :int
  (lattice mecab-lattice-t*))

(defcfun mecab-lattice-get-request-type :int
  (lattice mecab-lattice-t*))

(defcfun mecab-lattice-has-request-type :int
  (lattice mecab-lattice-t*)
  (request-type :int))

(defcfun mecab-lattice-set-request-type :void
  (lattice mecab-lattice-t*)
  (request-type :int))

(defcfun mecab-lattice-add-request-type :void
  (lattice mecab-lattice-t*)
  (request-type :int))

(defcfun mecab-lattice-remove-request-type :void
  (lattice mecab-lattice-t*)
  (request-type :int))

(defcfun mecab-lattice-new-node :void
  (lattice mecab-lattice-t*)
  (request-type :int))

(defcfun mecab-lattice-tostr :string
  (lattice mecab-lattice-t*))

(defcfun mecab-lattice-tostr2 :string
  (lattice mecab-lattice-t*)
  (n size-t))

(defcfun mecab-lattice-nbest-tostr :string
  (lattice mecab-lattice-t*)
  (n size-t))

(defcfun mecab-lattice-nbest-tostr2 :string
  (lattice mecab-lattice-t*)
  (n size-t)
  (buf char*)
  (size size-t))

(defcfun mecab-lattice-has-constraint :int
  (lattice mecab-lattice-t*))

(defcfun mecab-lattice-get-boundary-constraint :int
  (lattice mecab-lattice-t*)
  (pos size-t))

(defcfun mecab-lattice-get-feature-constraint :string
  (lattice mecab-lattice-t*)
  (pos size-t))

(defcfun mecab-lattice-set-boundary-constraint :void
  (lattice mecab-lattice-t*)
  (pos size-t)
  (boundary-type :int))

(defcfun mecab-lattice-set-feature-constraint :void
  (lattice mecab-lattice-t*)
  (begin-pos size-t)
  (end-pos size-t)
  (feature :string))

(defcfun mecab-lattice-set-result :void
  (lattice mecab-lattice-t*)
  (result :string))

(defcfun mecab-lattice-strerror :string
  (lattice mecab-lattice-t*))

(defcfun mecab-model-new mecab-model-t*
  (argc :int)
  (argv char**))

(defcfun mecab-model-new2 mecab-model-t*
  (arg :string))

(defcfun mecab-model-destroy :void
  (model mecab-model-t*))

(defcfun mecab-model-new-tagger mecab-t*
  (model mecab-model-t*))

(defcfun mecab-model-new-lattice mecab-lattice-t*
  (model mecab-model-t*))

(defcfun mecab-model-swap :int
  (model mecab-model-t*)
  (new-model mecab-model-t*))

(defcfun mecab-model-dictionary-info mecab-dictionary-info-t* 
  (model mecab-model-t*))

(defcfun mecab-model-transition-cost :int
  (model mecab-model-t*)
  (rcAttr :ushort)
  (lcAttr :ushort))

(defcfun mecab-model-lookup mecab-node-t*
  (model mecab-model-t*)
  (begin :string)
  (end :string)
  (lattice mecab-lattice-t*))

(defcfun mecab-do :int
  (argc :int)
  (argv char**))

(defcfun mecab-dict-index :int
  (argc :int)
  (argv char**))

(defcfun mecab-dict-gen :int
  (argc :int)
  (argv char**))

(defcfun mecab-cost-train :int
  (argc :int)
  (argv char**))

(defcfun mecab-system-eval :int
  (argc :int)
  (argv char**))

(defcfun mecab-test-gen :int
  (argc :int)
  (argv char**))
