#ifndef UTILS_H__
#define UTILS_H__


#include <R.h>
#include <Rinternals.h>


SEXP get_value(SEXP env, const char* name);

int get_int_value(SEXP env, const char* name);

void add_int_value(SEXP env, const char* name, int v);


#endif /* end of include guard: UTILS_H__ */
