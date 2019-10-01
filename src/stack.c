#include "stack.h"

SEXP stack_push(SEXP self, SEXP value) {
    PROTECT(value);
    SEXP q = Rf_findVarInFrame(self, Rf_install("q"));
    SEXP v;
    if (q == R_NilValue) {
        v = PROTECT(Rf_cons(value, R_NilValue));
        Rf_defineVar(Rf_install("q"), v, self);
    } else {
        v = PROTECT(Rf_cons(value, q));
        Rf_defineVar(Rf_install("q"), v, self);
    }
    UNPROTECT(2);
    return value;
}

SEXP stack_pop(SEXP self) {
    SEXP q = PROTECT(Rf_findVarInFrame(self, Rf_install("q")));
    if (q == R_NilValue) Rf_error("stack is empty");
    Rf_defineVar(Rf_install("q"), CDR(q), self);
    UNPROTECT(1);
    return CAR(q);
}
