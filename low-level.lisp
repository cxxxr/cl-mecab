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
+ mecab_dictionary_info_t
+ mecab_path_t
+ mecab_node_t
+ enum
  mecab_nor_node
  mecab_unk_node
  mecab_bos_node
  mecab_eos_node
  mecab_eon_node
+ enum
  mecab_sys_dic
  mecab_usr_dic
  mecab_unk_dic
+ enum
  mecab_one_best
  mecab_nbest
  mecab_partial
  mecab_marginal_prob
  mecab_alternative
  mecab_all_morphs
  mecab_allocate_sentence
+ enum
  mecab_any_boundary
  mecab_token_boundary
  mecab_inside_token
+ mecab_t
+ mecab_model_t
+ mecab_lattice_t
+ mecab_dictionary_info_t
+ mecab_node_t
+ mecab_path_t

+ mecab_new
+ mecab_new2
+ mecab_version
+ mecab_strerror
+ mecab_destroy
+ mecab_get_partial
+ mecab_set_partial
+ mecab_get_theta
+ mecab_set_theta
+ mecab_get_lattice_level
+ mecab_set_lattice_level
+ mecab_get_all_morphs
+ mecab_set_all_morphs
+ mecab_parse_lattice
+ mecab_sparse_tostr
- mecab_sparse_tostr2
- mecab_sparse_tostr3
- mecab_sparse_tonode
- mecab_sparse_tonode2
+ mecab_nbest_sparse_tostr
- mecab_nbest_sparse_tostr2
- mecab_nbest_sparse_tostr3
+ mecab_nbest_init
+ mecab_nbest_init2
+ mecab_nbest_next_tostr
- mecab_nbest_next_tostr2
- mecab_nbest_next_tonode
- mecab_format_node
- mecab_dictionary_info
- mecab_lattice_new
- mecab_lattice_destroy
- mecab_lattice_clear
- mecab_lattice_is_available
- mecab_lattice_get_bos_node
- mecab_lattice_get_eos_node
- mecab_lattice_get_all_begin_nodes
- mecab_lattice_get_all_end_nodes
- mecab_lattice_get_begin_nodes
- mecab_lattice_get_end_nodes
- mecab_lattice_get_sentence
- mecab_lattice_set_sentence
- mecab_lattice_set_sentence2
- mecab_lattice_get_size
- mecab_lattice_get_z
- mecab_lattice_set_z
- mecab_lattice_get_theta
- mecab_lattice_set_theta
- mecab_lattice_next
- mecab_lattice_get_request_type
- mecab_lattice_has_request_type
- mecab_lattice_set_request_type
- mecab_lattice_add_request_type
- mecab_lattice_remove_request_type
- mecab_lattice_new_node
- mecab_lattice_tostr
- mecab_lattice_tostr2
- mecab_lattice_nbest_tostr
- mecab_lattice_nbest_tostr2
- mecab_lattice_has_constraint
- mecab_lattice_get_boundary_constraint
- mecab_lattice_get_feature_constraint
- mecab_lattice_set_boundary_constraint
- mecab_lattice_set_feature_constraint
- mecab_lattice_set_result
- mecab_lattice_strerror
- mecab_model_new
- mecab_model_new2
- mecab_model_destroy
- mecab_model_new_tagger
- mecab_model_new_lattice
- mecab_model_swap
- mecab_dictionary_info_t
- mecab_model_transition_cost
- mecab_model_lookup
|#

#|
int           mecab_do(int argc, char **argv);
int           mecab_dict_index(int argc, char **argv);
int           mecab_dict_gen(int argc, char **argv);
int           mecab_cost_train(int argc, char **argv);
int           mecab_system_eval(int argc, char **argv);
int           mecab_test_gen(int argc, char **argv);
|#

(defctype mecab-dictionary-info-t* :pointer)
(defctype mecab-path-t* :pointer)
(defctype mecab-node-t* :pointer)

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

(defcfun mecab-nbest-sparse-tostr :string
  (mecab mecab-t*)
  (n size_t)
  (str :string))

(defcfun mecab-nbest-init :int
  (mecab mecab-t*)
  (str :string))

(defcfun mecab-nbest-init2 :int
  (mecab mecab-t*)
  (str :string)
  (len size_t))

(defcfun mecab-nbest-next-tostr :string
  (mecab mecab-t*))

#|
(loop :for form :in (uiop:read-file-forms "low-level.lisp")
      :when (eq (car form) 'defcfun)
      :collect (cadr form))
|#
