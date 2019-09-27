#ifndef _HEAP_H_
#define _HEAP_H_ 1

#include <R.h>
#include <Rinternals.h>

SEXP heapify(SEXP private);

SEXP heap_push(SEXP private, SEXP v, SEXP p);

SEXP heap_pop(SEXP private);

#endif
