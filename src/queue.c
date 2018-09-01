#include "queue.h"

SEXP queue_push(SEXP private, SEXP value) {
    PROTECT(value);
    SEXP q = Rf_findVarInFrame(private, Rf_install("q"));
    SEXP last;
    SEXP v;
    if (q == R_NilValue) {
        v = PROTECT(Rf_cons(value, R_NilValue));
        Rf_defineVar(Rf_install("q"), v, private);
        Rf_defineVar(Rf_install("last"), v, private);
    } else {
        last = PROTECT(Rf_findVarInFrame(private, Rf_install("last")));
        v = PROTECT(Rf_cons(value, R_NilValue));
        SETCDR(last, v);
        Rf_defineVar(Rf_install("last"), v, private);
        UNPROTECT(1);  // last
    }
    UNPROTECT(2);
    return value;
}

SEXP queue_pop(SEXP private) {
    SEXP q = PROTECT(Rf_findVarInFrame(private, Rf_install("q")));
    if (q == R_NilValue) Rf_error("queue is empty");
    Rf_defineVar(Rf_install("q"), CDR(q), private);
    UNPROTECT(1);
    return CAR(q);
}
