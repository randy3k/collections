#include "queue.h"
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
    if (last == NULL) {
        last = pairlist_last(q);
        R_SetExternalPtrAddr(last_ptr, last);
    }
    UNPROTECT(1);
    return last;
}


SEXP queue_push(SEXP self, SEXP value) {
    PROTECT(value);
    SEXP q = PROTECT(get_sexp_value(self, "q"));
    SEXP last_ptr = PROTECT(get_sexp_value(self, "last"));
    SEXP last;
    SEXP v;
    if (q == R_NilValue) {
        v = PROTECT(Rf_cons(value, R_NilValue));
        set_sexp_value(self, "q", v);
        R_SetExternalPtrAddr(last_ptr, v);
        UNPROTECT(1);
    } else {
        last = PROTECT(get_last_cons(q, last_ptr));
        v = PROTECT(Rf_cons(value, R_NilValue));
        SETCDR(last, v);
        R_SetExternalPtrAddr(last_ptr, v);
        UNPROTECT(2);
    }
    UNPROTECT(3);
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
