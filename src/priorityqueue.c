#include "priorityqueue.h"

static void swap(SEXP h, int a, int b) {
    SEXP temp = PROTECT(VECTOR_ELT(h, a));
    SET_VECTOR_ELT(h, a, VECTOR_ELT(h, b));
    SET_VECTOR_ELT(h, b, temp);
    UNPROTECT(1);
}

static int cmp(SEXP h, int a, int b) {
    SEXP _x = VECTOR_ELT(h, a);
    SEXP _y = VECTOR_ELT(h, b);
    double x = Rf_asReal(VECTOR_ELT(_x, 0));
    double y = Rf_asReal(VECTOR_ELT(_y, 0));
    return x < y;
}

static void ensure_capacity(SEXP private, int n) {
    SEXP h = PROTECT(Rf_findVarInFrame(private, Rf_install("h")));
    int m = Rf_length(h);
    int i;
    SEXP h2;
    if (m == 0) {
        h = PROTECT(Rf_allocVector(VECSXP, 16));
        Rf_defineVar(Rf_install("h"), h, private);
        UNPROTECT(1);
    } else if (m < n + 1) {
        h2 = PROTECT(Rf_allocVector(VECSXP, 2 * m));
        for (i = 0; i < n; i++) {
            SET_VECTOR_ELT(h2, i, VECTOR_ELT(h, i));
        }
        Rf_defineVar(Rf_install("h"), h2, private);
        UNPROTECT(1);
    }
    UNPROTECT(1);
}

static void sift_down(SEXP h, int start, int end) {
    int root = start;
    int child;
    while (root * 2 + 1 <= end) {
        child = root * 2 + 1;
        if (child + 1 <= end && cmp(h, child, child+1)) {
            child++;
        }
        if (cmp(h, root, child)) {
            swap(h, root, child);
            root = child;
        } else {
            return;
        }
    }
}

static void sift_up(SEXP h, int child) {
    int root = (child - 1) / 2;
    while (child > 0 && cmp(h, root, child)) {
        swap(h, root, child);
        child = root;
        root = (child - 1) / 2;
    }
}

SEXP heapify(SEXP private) {
    SEXP h = PROTECT(Rf_findVarInFrame(private, Rf_install("h")));
    SEXP _n = PROTECT(Rf_findVarInFrame(private, Rf_install("n")));
    int n = Rf_asInteger(_n);
    int start = (n - 2) / 2;
    while (start >= 0) {
        sift_down(h, start, n - 1);
        start = start - 1;
    }
    UNPROTECT(2);
    return h;
}

SEXP heap_push(SEXP private, SEXP v, SEXP p) {
    PROTECT(v);
    PROTECT(p);
    SEXP _n = Rf_findVarInFrame(private, Rf_install("n"));
    int n = Rf_asInteger(_n);
    ensure_capacity(private, n);
    SEXP h = PROTECT(Rf_findVarInFrame(private, Rf_install("h")));
    SEXP x = PROTECT(Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(x, 0, p);
    SET_VECTOR_ELT(x, 1, v);
    SET_VECTOR_ELT(h, n, x);
    sift_up(h, n);
    _n = PROTECT(Rf_ScalarInteger(n + 1));
    Rf_defineVar(Rf_install("n"), _n, private);
    UNPROTECT(5);
    return v;
}

SEXP heap_pop(SEXP private) {
    SEXP h = PROTECT(Rf_findVarInFrame(private, Rf_install("h")));
    SEXP _n = PROTECT(Rf_findVarInFrame(private, Rf_install("n")));
    int n = Rf_asInteger(_n);
    if (n == 0) Rf_error("queue is empty");
    SEXP x = PROTECT(VECTOR_ELT(h, 0));
    SET_VECTOR_ELT(h, 0, VECTOR_ELT(h, n - 1));
    sift_down(h, 0, n - 2);
    _n = PROTECT(Rf_ScalarInteger(n - 1));
    Rf_defineVar(Rf_install("n"), _n, private);
    UNPROTECT(4);
    return VECTOR_ELT(x, 1);
}
