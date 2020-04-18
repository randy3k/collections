#ifndef _dict_H_
#define _dict_H_ 1

#include <R.h>
#include <Rinternals.h>



SEXP dict_get(SEXP self, SEXP ht_xptr, SEXP _key, SEXP _default);

SEXP dict_set(SEXP self, SEXP ht_xptr, SEXP _key, SEXP value);

SEXP dict_remove(SEXP self, SEXP ht_xptr, SEXP _key);

SEXP dict_has(SEXP self, SEXP _key);

SEXP dict_keys(SEXP self);

SEXP dict_values(SEXP self);

SEXP dict_clear(SEXP self);

#endif
