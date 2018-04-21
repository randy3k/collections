#ifndef _QUEUE_H_
#define _QUEUE_H_ 1

#include <R.h>
#include <Rinternals.h>

SEXP queue_push(SEXP private, SEXP value);

SEXP queue_pop(SEXP private);

#endif
