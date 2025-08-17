#include "deque.h"
#include "utils.h"

#if !defined(static_inline)
#if defined(_MSC_VER) || defined(__GNUC__)
#define static_inline static __inline
#else
#define static_inline static
#endif
#endif


static_inline SEXP get_last_cons(SEXP q, SEXP last_ptr) {
    SEXP last = PROTECT(R_ExternalPtrAddr(last_ptr));
    SEXP nextq;
    if (last == NULL) {
        nextq = CDR(q);
        while (!Rf_isNull(nextq)) {
            R_SetExternalPtrAddr(VECTOR_ELT(CAR(nextq), 0), q);
            q = nextq;
            nextq = CDR(q);
        }
        R_SetExternalPtrAddr(last_ptr, q);
        last = q;
    }
    UNPROTECT(1);
    return last;
}


SEXP deque_push(SEXP self, SEXP value) {
    PROTECT(value);
    SEXP q = PROTECT(get_sexp_value(self, "q"));
    SEXP last_ptr = PROTECT(get_sexp_value(self, "last"));
    SEXP x = PROTECT(Rf_allocVector(VECSXP ,2));
    SEXP last = PROTECT(get_last_cons(q, last_ptr));
    SEXP v;
    if (q == R_NilValue) {
        SET_VECTOR_ELT(x, 0, R_NilValue);
        SET_VECTOR_ELT(x, 1, value);
        v = PROTECT(Rf_cons(x, R_NilValue));
        set_sexp_value(self, "q", v);
        R_SetExternalPtrAddr(last_ptr, v);
        UNPROTECT(1);
    } else {
        SET_VECTOR_ELT(x, 0, PROTECT(R_MakeExternalPtr(last, R_NilValue, R_NilValue)));
        SET_VECTOR_ELT(x, 1, value);
        v = PROTECT(Rf_cons(x, R_NilValue));
        SETCDR(last, v);
        R_SetExternalPtrAddr(last_ptr, v);
        UNPROTECT(2);
    }
    add_int_value(self, "n", 1);
    UNPROTECT(5);
    return value;
}


SEXP deque_pushleft(SEXP self, SEXP value) {
    PROTECT(value);
    SEXP q = PROTECT(get_sexp_value(self, "q"));
    SEXP last_ptr = PROTECT(get_sexp_value(self, "last"));
    SEXP x = PROTECT(Rf_allocVector(VECSXP ,2));
    SEXP v;
    if (q == R_NilValue) {
        SET_VECTOR_ELT(x, 0, R_NilValue);
        SET_VECTOR_ELT(x, 1, value);
        v = PROTECT(Rf_cons(x, R_NilValue));
        set_sexp_value(self, "q", v);
        R_SetExternalPtrAddr(last_ptr, v);
        UNPROTECT(1);
    } else {
        SET_VECTOR_ELT(x, 0, R_NilValue);
        SET_VECTOR_ELT(x, 1, value);
        v = PROTECT(Rf_cons(x, q));
        SET_VECTOR_ELT(CAR(q), 0, PROTECT(R_MakeExternalPtr(v, R_NilValue, R_NilValue)));
        set_sexp_value(self, "q", v);
        UNPROTECT(2);
    }
    add_int_value(self, "n", 1);
    UNPROTECT(4);
    return value;
}


SEXP deque_pop(SEXP self) {
    SEXP q = PROTECT(get_sexp_value(self, "q"));
    if (q == R_NilValue) Rf_error("deque is empty");
    SEXP last_ptr = PROTECT(get_sexp_value(self, "last"));
    SEXP last = PROTECT(get_last_cons(q, last_ptr));
    SEXP prev_ptr = VECTOR_ELT(CAR(last), 0);
    if (prev_ptr == R_NilValue) {
        set_sexp_value(self, "q", R_NilValue);
        R_SetExternalPtrAddr(last_ptr, NULL);
    } else {
        SEXP prev = R_ExternalPtrAddr(prev_ptr);
        R_SetExternalPtrAddr(last_ptr, prev);
        SETCDR(prev, R_NilValue);
    }
    add_int_value(self, "n", -1);
    UNPROTECT(3);
    return VECTOR_ELT(CAR(last), 1);
}

SEXP deque_popleft(SEXP self) {
    SEXP q = PROTECT(get_sexp_value(self, "q"));
    if (q == R_NilValue) Rf_error("deque is empty");
    SEXP nextq = CDR(q);
    if (nextq == R_NilValue) {
        set_sexp_value(self, "q", nextq);
        SEXP last_ptr = PROTECT(get_sexp_value(self, "last"));
        R_SetExternalPtrAddr(last_ptr, NULL);
        UNPROTECT(1);
    } else {
        set_sexp_value(self, "q", nextq);
        SET_VECTOR_ELT(CAR(nextq), 0, R_NilValue);
    }
    add_int_value(self, "n", -1);
    UNPROTECT(1);
    return VECTOR_ELT(CAR(q), 1);
}


SEXP deque_peek(SEXP self) {
    SEXP last_ptr = PROTECT(get_sexp_value(self, "last"));
    SEXP q = PROTECT(get_sexp_value(self, "q"));
    if (Rf_isNull(q)) {
        Rf_error("deque is empty");
    }
    SEXP last = PROTECT(get_last_cons(q, last_ptr));
    SEXP value = VECTOR_ELT(CAR(last), 1);
    UNPROTECT(3);
    return value;
}


SEXP deque_remove(SEXP self, SEXP value) {
    SEXP q = PROTECT(get_sexp_value(self, "q"));
    SEXP last_ptr = PROTECT(get_sexp_value(self, "last"));
    // make sure the pointers are resolved after serialization/unserialization
    get_last_cons(q, last_ptr);
    SEXP v, v1, nextq, prev_ptr;
    while (q != R_NilValue) {
        v = CAR(q);
        v1 = PROTECT(VECTOR_ELT(v, 1));
        nextq = CDR(q);
        if (R_compute_identical(v1, value, 16)) {
            prev_ptr = PROTECT(VECTOR_ELT(v, 0));
            if (nextq == R_NilValue && prev_ptr == R_NilValue) {
                set_sexp_value(self, "q", R_NilValue);
                R_SetExternalPtrAddr(last_ptr, NULL);
            } else if (nextq == R_NilValue) {
                // last item
                SEXP prev = R_ExternalPtrAddr(prev_ptr);
                SETCDR(prev, R_NilValue);
                R_SetExternalPtrAddr(last_ptr, prev);
            } else if (prev_ptr == R_NilValue) {
                // first item
                SET_VECTOR_ELT(CAR(nextq), 0, R_NilValue);
                set_sexp_value(self, "q", nextq);
            } else {
                SEXP prev = R_ExternalPtrAddr(prev_ptr);
                SETCDR(prev, nextq);
                SET_VECTOR_ELT(CAR(nextq), 0, prev_ptr);
            }
            UNPROTECT(4);
            add_int_value(self, "n", -1);
            return R_NilValue;
        }
        UNPROTECT(1);
        q = nextq;
    }
    UNPROTECT(2);
    Rf_error("value not found");
    return R_NilValue;
}


SEXP deque_clear(SEXP self) {
    set_sexp_value(self, "q", R_NilValue);
    SEXP last = PROTECT(R_MakeExternalPtr(NULL, R_NilValue, R_NilValue));
    set_sexp_value(self, "last", last);
    UNPROTECT(1);
    set_int_value(self, "n", 0);
    return R_NilValue;
}
