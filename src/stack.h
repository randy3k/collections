#ifndef _STACK_H_
#define _STACK_H_ 1

#include <R.h>
#include <Rinternals.h>

SEXP stack_push(SEXP private, SEXP value);

SEXP stack_pop(SEXP private);

#endif
