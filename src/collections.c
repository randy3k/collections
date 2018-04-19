#include <R.h>
#include <Rinternals.h>
#include "pairlist.h"

static const R_CallMethodDef CallEntries[] = {
    {"pairlist_car", (DL_FUNC) &pairlist_car, 1},
    {"pairlist_cdr", (DL_FUNC) &pairlist_cdr, 1},
    {"pairlist_setcar", (DL_FUNC) &pairlist_setcar, 2},
    {"pairlist_setcdr", (DL_FUNC) &pairlist_setcdr, 2},
    {"pairlist_append", (DL_FUNC) &pairlist_append, 2},
    {"pairlist_prepend", (DL_FUNC) &pairlist_prepend, 2},
    {NULL, NULL, 0}
};

void R_init_collections(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
