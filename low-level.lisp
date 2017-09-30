(defpackage #:mecab.low-level
  (:use #:cl #:cffi)
  (:export
   #:%mecab_new
   #:%mecab_new2
   #:%mecab_version
   #:%mecab_destroy
   #:%mecab_sparse_tostr
   #:%mecab_nbest_sparse_tostr))

(in-package #:mecab.low-level)

(defctype size_t :uint)

(define-foreign-library libmecab
  (:unix "libmecab.so")
  (:darwin "libmecab.dylib")
  (:windows "libmecab.dll"))

(use-foreign-library libmecab)

#|
- mecab_dictionary_info_t
- mecab_path_t
- mecab_node_t
- enum
  mecab_nor_node
  mecab_unk_node
  mecab_bos_node
  mecab_eos_node
  mecab_eon_node
- enum
  mecab_sys_dic
  mecab_usr_dic
  mecab_unk_dic
- enum
  mecab_one_best
  mecab_nbest
  mecab_partial
  mecab_marginal_prob
  mecab_alternative
  mecab_all_morphs
  mecab_allocate_sentence
- enum
  mecab_any_boundary
  mecab_token_boundary
  mecab_inside_token
- mecab_t
- mecab_model_t
- mecab_lattice_t
- mecab_dictionary_info_t
- mecab_node_t
- mecab_path_t

+ mecab_new
+ mecab_new2
+ mecab_version
- mecab_strerror
+ mecab_destroy
- mecab_get_partial
- mecab_set_partial
- mecab_get_theta
- mecab_set_theta
- mecab_get_lattice_level
- mecab_set_lattice_level
- mecab_get_all_morphs
- mecab_set_all_morphs
- mecab_parse_lattice
+ mecab_sparse_tostr
- mecab_sparse_tostr2
- mecab_sparse_tostr3
- mecab_sparse_tonode
- mecab_sparse_tonode2
+ mecab_nbest_sparse_tostr
- mecab_nbest_sparse_tostr2
- mecab_nbest_sparse_tostr3
- mecab_nbest_init
- mecab_nbest_init2
- mecab_nbest_next_tostr
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

(defcfun ("mecab_new" %mecab_new) :pointer
  (argc :int)
  (argv :string+ptr))

(defcfun ("mecab_new2" %mecab_new2) :pointer
  (arg :string))

(defcfun ("mecab_version" %mecab_version) :string)

(defcfun ("mecab_destroy" %mecab_destroy) :void
  (mecab :pointer))

(defcfun ("mecab_sparse_tostr" %mecab_sparse_tostr) :string
  (mecab :pointer)
  (str :string))

(defcfun ("mecab_nbest_sparse_tostr" %mecab_nbest_sparse_tostr) :string
  (mecab :pointer)
  (n size_t)
  (str :string))
