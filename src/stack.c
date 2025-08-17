#include "stack.h"
#include "utils.h"


SEXP stack_push(SEXP self, SEXP value) {
    PROTECT(value);
    SEXP q = get_sexp_value(self, "q");
    SEXP v;
    if (q == R_NilValue) {
        v = PROTECT(Rf_cons(value, R_NilValue));
        set_sexp_value(self, "q", v);
    } else {
        v = PROTECT(Rf_cons(value, q));
        set_sexp_value(self, "q", v);
    }
    add_int_value(self, "n", 1);
    UNPROTECT(2);
    return value;
}

SEXP stack_pop(SEXP self) {
    SEXP q = PROTECT(get_sexp_value(self, "q"));
    if (q == R_NilValue) Rf_error("stack is empty");
    set_sexp_value(self, "q", CDR(q));
    add_int_value(self, "n", -1);
    UNPROTECT(1);
    return CAR(q);
}
