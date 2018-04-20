#include "heap.h"

static void swap(SEXP h, int a, int b) {
    SEXP temp = PROTECT(Rf_duplicate(VECTOR_ELT(h, a)));
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

static SEXP ensure_capacity(SEXP h, int n) {
    int m = Rf_length(h);
    int i;
    SEXP h2;
    if (m == 0) {
        h = Rf_allocVector(VECSXP, 16);
    } else if (m < n + 1) {
        h2 = Rf_allocVector(VECSXP, 2 * m);
        for (i = 0; i < n; i++) {
            SET_VECTOR_ELT(h2, i, VECTOR_ELT(h, i));
        }
        h = h2;
    }
    return h;
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

SEXP heapify(SEXP h, SEXP _n) {
    if (TYPEOF(h) != VECSXP) {
        Rf_error("exptect an R list");
    }
    int n = Rf_asInteger(_n);
    int start = (n - 2) / 2;
    while (start >= 0) {
        sift_down(h, start, n - 1);
        start = start - 1;
    }
    return h;
}

SEXP heap_push(SEXP h, SEXP _n, SEXP v, SEXP p) {
    PROTECT(v);
    PROTECT(p);
    int n = Rf_asInteger(_n);
    h = ensure_capacity(h, n);
    SEXP x = PROTECT(Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(x, 0, p);
    SET_VECTOR_ELT(x, 1, v);
    SET_VECTOR_ELT(h, n, x);
    sift_up(h, n);
    UNPROTECT(3);
    return h;
}

SEXP heap_pop(SEXP h, SEXP _n) {
    int n = Rf_asInteger(_n);
    swap(h, 0, n - 1);
    SEXP x = PROTECT(VECTOR_ELT(h, n - 1));
    sift_down(h, 0, n - 2);
    UNPROTECT(1);
    return VECTOR_ELT(x, 1);
}
