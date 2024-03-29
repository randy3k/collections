#include "priority_queue.h"
#include "utils.h"

#define INITIAL_SIZE 16
#define GROW_FACTOR 1.5
#define SHRINK_FACTOR 0.15


static void swap(SEXP h, int a, int b) {
    SEXP ha = PROTECT(VECTOR_ELT(h, a));
    SEXP hb = PROTECT(VECTOR_ELT(h, b));
    SET_VECTOR_ELT(h, a, hb);
    SET_VECTOR_ELT(h, b, ha);
    UNPROTECT(2);
}

static int cmp(SEXP h, int a, int b) {
    PROTECT(h);
    SEXP _x = PROTECT(VECTOR_ELT(h, a));
    SEXP _y = PROTECT(VECTOR_ELT(h, b));
    double x = Rf_asReal(VECTOR_ELT(_x, 0));
    double y = Rf_asReal(VECTOR_ELT(_y, 0));
    UNPROTECT(3);
    return x < y;
}

static void grow(SEXP self, int n) {
    SEXP h = PROTECT(get_sexp_value(self, "h"));
    int m = Rf_length(h);
    int i;
    SEXP h2;
    if (m == 0) {
        h = PROTECT(Rf_allocVector(VECSXP, INITIAL_SIZE));
        set_sexp_value(self, "h", h);
        UNPROTECT(1);
    } else if (m < n + 1) {
        h2 = PROTECT(Rf_allocVector(VECSXP, (int) ceil(GROW_FACTOR * m)));
        for (i = 0; i < n; i++) {
            SEXP h2i = PROTECT(VECTOR_ELT(h, i));
            SET_VECTOR_ELT(h2, i, h2i);
            UNPROTECT(1);
        }
        set_sexp_value(self, "h", h2);
        UNPROTECT(1);
    }
    UNPROTECT(1);
}

static void shrink(SEXP self, int n) {
    SEXP h = PROTECT(get_sexp_value(self, "h"));
    int m = Rf_length(h);
    int m1 = ceil(SHRINK_FACTOR * m);
    int i;
    SEXP h2;
    if (n < m1 && m1 > INITIAL_SIZE) {
        h2 = PROTECT(Rf_allocVector(VECSXP, m1));
        for (i = 0; i < n; i++) {
            SEXP h2i = PROTECT(VECTOR_ELT(h, i));
            SET_VECTOR_ELT(h2, i, h2i);
            UNPROTECT(1);
        }
        set_sexp_value(self, "h", h2);
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

SEXP heapify(SEXP self) {
    SEXP h = PROTECT(get_sexp_value(self, "h"));
    int n = get_int_value(self, "n");
    int start = (n - 2) / 2;
    while (start >= 0) {
        sift_down(h, start, n - 1);
        start = start - 1;
    }
    UNPROTECT(1);
    return h;
}

SEXP heap_push(SEXP self, SEXP v, SEXP p) {
    PROTECT(v);
    PROTECT(p);
    SEXP _n = get_sexp_value(self, "n");
    int n = Rf_asInteger(_n);
    grow(self, n);
    SEXP h = PROTECT(get_sexp_value(self, "h"));
    SEXP x = PROTECT(Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(x, 0, p);
    SET_VECTOR_ELT(x, 1, v);
    SET_VECTOR_ELT(h, n, x);
    sift_up(h, n);
    _n = PROTECT(Rf_ScalarInteger(n + 1));
    set_sexp_value(self, "n", _n);
    UNPROTECT(5);
    return v;
}

SEXP heap_pop(SEXP self) {
    SEXP h = PROTECT(get_sexp_value(self, "h"));
    SEXP _n = PROTECT(get_sexp_value(self, "n"));
    int n = Rf_asInteger(_n);
    if (n == 0) Rf_error("queue is empty");
    SEXP x = PROTECT(VECTOR_ELT(h, 0));
    SEXP h0 = PROTECT(VECTOR_ELT(h, n - 1));
    SET_VECTOR_ELT(h, 0, h0);
    sift_down(h, 0, n - 2);
    _n = PROTECT(Rf_ScalarInteger(n - 1));
    set_sexp_value(self, "n", _n);
    shrink(self, n);
    SEXP res = PROTECT(VECTOR_ELT(x, 1));
    UNPROTECT(6);
    return res;
}
