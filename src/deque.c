#include "deque.h"

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


SEXP deque_push(SEXP private, SEXP value) {
    SEXP q = Rf_findVarInFrame(private, Rf_install("q"));
    SEXP last;
    SEXP v;
    SEXP x = PROTECT(Rf_allocVector(VECSXP ,2));
    if (q == R_NilValue) {
        SET_VECTOR_ELT(x, 0, R_NilValue);
        SET_VECTOR_ELT(x, 1, value);
        v = PROTECT(Rf_cons(x, R_NilValue));
        Rf_defineVar(Rf_install("q"), v, private);
        Rf_defineVar(Rf_install("last"), v, private);
    } else {
        last = Rf_findVarInFrame(private, Rf_install("last"));
        SET_VECTOR_ELT(x, 0, last);
        SET_VECTOR_ELT(x, 1, value);
        v = PROTECT(Rf_cons(x, R_NilValue));
        SETCDR(last, v);
        Rf_defineVar(Rf_install("last"), v, private);
    }
    UNPROTECT(2);
    return value;
}

SEXP deque_pushleft(SEXP private, SEXP value) {
    SEXP q = Rf_findVarInFrame(private, Rf_install("q"));
    SEXP v;
    SEXP x = PROTECT(Rf_allocVector(VECSXP ,2));
    if (q == R_NilValue) {
        SET_VECTOR_ELT(x, 0, R_NilValue);
        SET_VECTOR_ELT(x, 1, value);
        v = PROTECT(Rf_cons(x, R_NilValue));
        Rf_defineVar(Rf_install("q"), v, private);
        Rf_defineVar(Rf_install("last"), v, private);
    } else {
        SET_VECTOR_ELT(x, 0, R_NilValue);
        SET_VECTOR_ELT(x, 1, value);
        v = PROTECT(Rf_cons(x, q));
        SET_VECTOR_ELT(CAR(q), 0, v);
        Rf_defineVar(Rf_install("q"), v, private);
    }
    UNPROTECT(2);
    return value;
}

SEXP deque_pop(SEXP private) {
    SEXP last = Rf_findVarInFrame(private, Rf_install("last"));
    if (last == R_NilValue) Rf_error("deque is empty");
    SEXP prev = VECTOR_ELT(CAR(last), 0);
    if (prev == R_NilValue) {
        Rf_defineVar(Rf_install("q"), R_NilValue, private);
    } else {
        SETCDR(prev, R_NilValue);
    }
    Rf_defineVar(Rf_install("last"), prev, private);
    return VECTOR_ELT(CAR(last), 1);
}

SEXP deque_popleft(SEXP private) {
    SEXP q = Rf_findVarInFrame(private, Rf_install("q"));
    if (q == R_NilValue) Rf_error("deque is empty");
    SEXP nextq = CDR(q);
    if (nextq == R_NilValue) {
        Rf_defineVar(Rf_install("last"), R_NilValue, private);
    } else {
        SET_VECTOR_ELT(CAR(nextq), 0, R_NilValue);
    }
    Rf_defineVar(Rf_install("q"), nextq, private);
    return VECTOR_ELT(CAR(q), 1);
}

SEXP deque_remove(SEXP private, SEXP value) {
    SEXP q = Rf_findVarInFrame(private, Rf_install("q"));
    SEXP v, nextq, prev;
    while (q != R_NilValue) {
        v = CAR(q);
        nextq = CDR(q);
        if (R_compute_identical(VECTOR_ELT(v, 1), value, 16)) {
            prev = VECTOR_ELT(v, 0);
            if (nextq == R_NilValue && prev == R_NilValue) {
                Rf_defineVar(Rf_install("q"), R_NilValue, private);
                Rf_defineVar(Rf_install("last"), R_NilValue, private);
            } else if (nextq == R_NilValue) {
                SETCDR(prev, R_NilValue);
                Rf_defineVar(Rf_install("last"), prev, private);
            } else if (prev == R_NilValue) {
                Rf_defineVar(Rf_install("q"), nextq, private);
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
