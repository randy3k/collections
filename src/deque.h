#ifndef _deque_H_
#define _deque_H_ 1

#include <R.h>
#include <Rinternals.h>

SEXP deque_push(SEXP self, SEXP value);

SEXP deque_pushleft(SEXP self, SEXP value);

SEXP deque_pop(SEXP self);

SEXP deque_popleft(SEXP self);

SEXP deque_peek(SEXP self);

SEXP deque_remove(SEXP self, SEXP value);

SEXP deque_clear(SEXP self);

#endif
