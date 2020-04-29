#include "queue.h"
#include "utils.h"


SEXP queue_push(SEXP self, SEXP value) {
    PROTECT(value);
    SEXP q = get_sexp_value(self, "q");
    SEXP last_ptr, last;
    SEXP v;
    if (q == R_NilValue) {
        v = PROTECT(Rf_cons(value, R_NilValue));
        set_sexp_value(self, "q", v);
        R_SetExternalPtrAddr(get_sexp_value(self, "last"), v);
        UNPROTECT(1);
    } else {
        last_ptr = PROTECT(get_sexp_value(self, "last"));
        last = PROTECT(R_ExternalPtrAddr(last_ptr));
        if (last == NULL) {
            last = pairlist_last(q);
            R_SetExternalPtrAddr(get_sexp_value(self, "last"), last);
        }
        v = PROTECT(Rf_cons(value, R_NilValue));
        SETCDR(last, v);
        R_SetExternalPtrAddr(last_ptr, v);
        UNPROTECT(3);  // last
    }
    UNPROTECT(1);
    return value;
}

SEXP queue_pop(SEXP self) {
    SEXP q = PROTECT(get_sexp_value(self, "q"));
    if (q == R_NilValue) Rf_error("queue is empty");
    set_sexp_value(self, "q", CDR(q));
    UNPROTECT(1);
    return CAR(q);
}


SEXP queue_clear(SEXP self) {
    set_sexp_value(self, "q", R_NilValue);
    SEXP last = PROTECT(R_MakeExternalPtr(NULL, R_NilValue, R_NilValue));
    set_sexp_value(self, "last", last);
    UNPROTECT(1);
    return R_NilValue;
}
