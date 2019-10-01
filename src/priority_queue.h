#ifndef _HEAP_H_
#define _HEAP_H_ 1

#include <R.h>
#include <Rinternals.h>

SEXP heapify(SEXP self);

SEXP heap_push(SEXP self, SEXP v, SEXP p);

SEXP heap_pop(SEXP self);

#endif
