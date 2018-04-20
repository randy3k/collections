#ifndef _HEAP_H_
#define _HEAP_H_ 1

#include <R.h>
#include <Rinternals.h>

SEXP heapify(SEXP h, SEXP _n);

SEXP heap_push(SEXP h, SEXP _n, SEXP v, SEXP p);

SEXP heap_pop(SEXP h, SEXP _n);

#endif
