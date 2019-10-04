#ifndef _DICT_H_
#define _DICT_H_ 1

#include <R.h>
#include <Rinternals.h>


SEXP dict_index_get(SEXP self, SEXP ht_xptr, SEXP _key);

SEXP dict_set(SEXP self, SEXP ht_xptr, SEXP _key, SEXP value);

SEXP dict_remove(SEXP self, SEXP ht_xptr, SEXP _key);

#endif
