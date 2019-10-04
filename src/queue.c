#include "queue.h"
#include "utils.h"


SEXP queue_push(SEXP self, SEXP value) {
    PROTECT(value);
    SEXP q = get_sexp_value(self, "q");
    SEXP last;
    SEXP v;
    if (q == R_NilValue) {
        v = PROTECT(Rf_cons(value, R_NilValue));
        set_sexp_value(self, "q", v);
        set_sexp_value(self, "last", v);
    } else {
        last = PROTECT(get_sexp_value(self, "last"));
        v = PROTECT(Rf_cons(value, R_NilValue));
        SETCDR(last, v);
        set_sexp_value(self, "last", v);
        UNPROTECT(1);  // last
    }
    UNPROTECT(2);
    return value;
}

SEXP queue_pop(SEXP self) {
    SEXP q = PROTECT(get_sexp_value(self, "q"));
    if (q == R_NilValue) Rf_error("queue is empty");
    set_sexp_value(self, "q", CDR(q));
    UNPROTECT(1);
    return CAR(q);
}
