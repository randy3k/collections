#include "stack.h"

SEXP stack_push(SEXP private, SEXP value) {
    PROTECT(value);
    SEXP q = Rf_findVarInFrame(private, Rf_install("q"));
    SEXP v;
    if (q == R_NilValue) {
        v = PROTECT(Rf_cons(value, R_NilValue));
        Rf_defineVar(Rf_install("q"), v, private);
    } else {
        v = PROTECT(Rf_cons(value, q));
        Rf_defineVar(Rf_install("q"), v, private);
    }
    UNPROTECT(2);
    return value;
}

SEXP stack_pop(SEXP private) {
    SEXP q = PROTECT(Rf_findVarInFrame(private, Rf_install("q")));
    if (q == R_NilValue) Rf_error("stack is empty");
    Rf_defineVar(Rf_install("q"), CDR(q), private);
    UNPROTECT(1);
    return CAR(q);
}
