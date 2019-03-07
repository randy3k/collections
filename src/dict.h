#ifndef _DICT_H_
#define _DICT_H_ 1

#include <R.h>
#include <Rinternals.h>

SEXP dict_get(SEXP e, SEXP key, SEXP _default);

#endif
