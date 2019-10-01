#include "queue.h"

SEXP queue_push(SEXP self, SEXP value) {
    PROTECT(value);
    SEXP q = Rf_findVarInFrame(self, Rf_install("q"));
    SEXP last;
    SEXP v;
    if (q == R_NilValue) {
        v = PROTECT(Rf_cons(value, R_NilValue));
        Rf_defineVar(Rf_install("q"), v, self);
        Rf_defineVar(Rf_install("last"), v, self);
    } else {
        last = PROTECT(Rf_findVarInFrame(self, Rf_install("last")));
        v = PROTECT(Rf_cons(value, R_NilValue));
        SETCDR(last, v);
        Rf_defineVar(Rf_install("last"), v, self);
        UNPROTECT(1);  // last
    }
    UNPROTECT(2);
    return value;
}

SEXP queue_pop(SEXP self) {
    SEXP q = PROTECT(Rf_findVarInFrame(self, Rf_install("q")));
    if (q == R_NilValue) Rf_error("queue is empty");
    Rf_defineVar(Rf_install("q"), CDR(q), self);
    UNPROTECT(1);
    return CAR(q);
}
