#ifndef UTILS_H__
#define UTILS_H__


#include <R.h>
#include <Rinternals.h>

SEXP r_current_frame();

int r_is_missing(SEXP env, const char* name);

SEXP pairlist_car(SEXP x);

SEXP pairlist_cdr(SEXP x);

SEXP pairlist_last(SEXP x);

SEXP get_sexp_value(SEXP env, const char* name);

void set_sexp_value(SEXP env, const char* name, SEXP value);

int get_int_value(SEXP env, const char* name);

void set_int_value(SEXP env, const char* name, int v);

int add_int_value(SEXP env, const char* name, int v);


#endif /* end of include guard: UTILS_H__ */
