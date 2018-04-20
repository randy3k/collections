#include "deque.h"

SEXP deque_remove(SEXP private, SEXP value) {
    SEXP q = Rf_findVarInFrame(private, Rf_install("q"));
    SEXP v, nextq, prev;
    while (q != R_NilValue) {
        v = CAR(q);
        nextq = CDR(q);
        if (R_compute_identical(VECTOR_ELT(v, 1), value, 16)) {
            prev = VECTOR_ELT(v, 0);
            if (nextq == R_NilValue) {
                Rf_defineVar(Rf_install("last"), prev, private);
                SETCDR(prev, R_NilValue);
            }
            if (prev == R_NilValue) {
                SET_VECTOR_ELT(CAR(nextq), 0, R_NilValue);
                Rf_defineVar(Rf_install("q"), nextq, private);
            } else {
                SETCDR(prev, nextq);
            }
            return R_NilValue;
        }
        q = nextq;
    }
    Rf_error("value not found");
    return R_NilValue;
}
