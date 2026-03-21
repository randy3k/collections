#ifndef UTILS_H__
#define UTILS_H__


#include <R.h>
#include <Rinternals.h>
#include <Rversion.h>

/* Backfill for R < 4.5.0 */
#if R_VERSION < R_Version(4, 5, 0)
static inline SEXP R_getVarEx(SEXP symbol, SEXP rho, int inherits, SEXP deflt) {
    SEXP ans = inherits ? Rf_findVar(symbol, rho) : Rf_findVarInFrame(rho, symbol);
    return (ans == R_UnboundValue) ? deflt : ans;
}
#endif

SEXP r_current_frame(void);

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
