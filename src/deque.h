#ifndef _DEQUE_H_
#define _DEQUE_H_ 1

#include <R.h>
#include <Rinternals.h>

SEXP pairlist_car(SEXP x);

SEXP pairlist_cdr(SEXP x);

SEXP deque_push(SEXP private, SEXP value);

SEXP deque_pushleft(SEXP private, SEXP value);

SEXP deque_pop(SEXP private);

SEXP deque_popleft(SEXP private);

SEXP deque_remove(SEXP private, SEXP value);

#endif
