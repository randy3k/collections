#include "deque.h"
#include "utils.h"

// return the current item of a pairlist
SEXP pairlist_car(SEXP x) {
  if (!Rf_isList(x))
    Rf_error("x must be a pairlist");
  return CAR(x);
}


// return the next item of a pairlist
SEXP pairlist_cdr(SEXP x) {
  if (!Rf_isList(x))
    Rf_error("x must be a pairlist");
  return CDR(x);
}


SEXP deque_push(SEXP self, SEXP value) {
    SEXP q = PROTECT(get_sexp_value(self, "q"));
    SEXP last;
    SEXP v;
    SEXP x = PROTECT(Rf_allocVector(VECSXP ,2));
    if (q == R_NilValue) {
        SET_VECTOR_ELT(x, 0, R_NilValue);
        SET_VECTOR_ELT(x, 1, value);
        v = PROTECT(Rf_cons(x, R_NilValue));
        set_sexp_value(self, "q", v);
        set_sexp_value(self, "last", v);
    } else {
        last = get_sexp_value(self, "last");
        SET_VECTOR_ELT(x, 0, last);
        SET_VECTOR_ELT(x, 1, value);
        v = PROTECT(Rf_cons(x, R_NilValue));
        SETCDR(last, v);
        set_sexp_value(self, "last", v);
    }
    UNPROTECT(3);
    return value;
}

SEXP deque_pushleft(SEXP self, SEXP value) {
    SEXP q = PROTECT(get_sexp_value(self, "q"));
    SEXP v;
    SEXP x = PROTECT(Rf_allocVector(VECSXP ,2));
    if (q == R_NilValue) {
        SET_VECTOR_ELT(x, 0, R_NilValue);
        SET_VECTOR_ELT(x, 1, value);
        v = PROTECT(Rf_cons(x, R_NilValue));
        set_sexp_value(self, "q", v);
        set_sexp_value(self, "last", v);
    } else {
        SET_VECTOR_ELT(x, 0, R_NilValue);
        SET_VECTOR_ELT(x, 1, value);
        v = PROTECT(Rf_cons(x, q));
        SET_VECTOR_ELT(CAR(q), 0, v);
        set_sexp_value(self, "q", v);
    }
    UNPROTECT(3);
    return value;
}

SEXP deque_pop(SEXP self) {
    SEXP last = PROTECT(get_sexp_value(self, "last"));
    if (last == R_NilValue) Rf_error("deque is empty");
    SEXP prev = VECTOR_ELT(CAR(last), 0);
    if (prev == R_NilValue) {
        set_sexp_value(self, "q", R_NilValue);
    } else {
        SETCDR(prev, R_NilValue);
    }
    set_sexp_value(self, "last", prev);
    UNPROTECT(1);
    return VECTOR_ELT(CAR(last), 1);
}

SEXP deque_popleft(SEXP self) {
    SEXP q = PROTECT(get_sexp_value(self, "q"));
    if (q == R_NilValue) Rf_error("deque is empty");
    SEXP nextq = CDR(q);
    if (nextq == R_NilValue) {
        set_sexp_value(self, "last", R_NilValue);
    } else {
        SET_VECTOR_ELT(CAR(nextq), 0, R_NilValue);
    }
    set_sexp_value(self, "q", nextq);
    UNPROTECT(1);
    return VECTOR_ELT(CAR(q), 1);
}

SEXP deque_remove(SEXP self, SEXP value) {
    SEXP q = get_sexp_value(self, "q");
    SEXP v, nextq, prev;
    while (q != R_NilValue) {
        v = CAR(q);
        nextq = CDR(q);
        if (R_compute_identical(VECTOR_ELT(v, 1), value, 16)) {
            prev = VECTOR_ELT(v, 0);
            if (nextq == R_NilValue && prev == R_NilValue) {
                set_sexp_value(self, "q", R_NilValue);
                set_sexp_value(self, "last", R_NilValue);
            } else if (nextq == R_NilValue) {
                SETCDR(prev, R_NilValue);
                set_sexp_value(self, "last", prev);
            } else if (prev == R_NilValue) {
                set_sexp_value(self, "q", nextq);
                SET_VECTOR_ELT(CAR(nextq), 0, R_NilValue);
            } else {
                SETCDR(prev, nextq);
                SET_VECTOR_ELT(CAR(nextq), 0, prev);
            }
            return R_NilValue;
        }
        q = nextq;
    }
    Rf_error("value not found");
    return R_NilValue;
}
