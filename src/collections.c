#include <R.h>
#include <Rinternals.h>
#include "queue.h"
#include "stack.h"
#include "deque.h"
#include "dict.h"
#include "priority_queue.h"
#include "utils.h"


SEXP missing_arg() {
    return R_MissingArg;
}

static const R_CallMethodDef CallEntries[] = {
    {"queue_push", (DL_FUNC) &queue_push, 2},
    {"queue_pop", (DL_FUNC) &queue_pop, 1},
    {"queue_clear", (DL_FUNC) &queue_clear, 1},
    {"stack_push", (DL_FUNC) &stack_push, 2},
    {"stack_pop", (DL_FUNC) &stack_pop, 1},
    {"pairlist_car", (DL_FUNC) &pairlist_car, 1},
    {"pairlist_cdr", (DL_FUNC) &pairlist_cdr, 1},
    {"deque_push", (DL_FUNC) &deque_push, 2},
    {"deque_pushleft", (DL_FUNC) &deque_pushleft, 2},
    {"deque_pop", (DL_FUNC) &deque_pop, 1},
    {"deque_popleft", (DL_FUNC) &deque_popleft, 1},
    {"deque_peek", (DL_FUNC) &deque_peek, 1},
    {"deque_remove", (DL_FUNC) &deque_remove, 2},
    {"deque_clear", (DL_FUNC) &deque_clear, 1},
    {"dict_hash", (DL_FUNC) &dict_hash, 1},
    {"dict_get", (DL_FUNC) &dict_get, 2},
    {"dict_set", (DL_FUNC) &dict_set, 3},
    {"dict_remove", (DL_FUNC) &dict_remove, 3},
    {"dict_has", (DL_FUNC) &dict_has, 2},
    {"dict_keys", (DL_FUNC) &dict_keys, 1},
    {"dict_values", (DL_FUNC) &dict_values, 1},
    {"dict_clear", (DL_FUNC) &dict_clear, 1},
    {"heapify", (DL_FUNC) &heapify, 1},
    {"heap_push", (DL_FUNC) &heap_push, 3},
    {"heap_pop", (DL_FUNC) &heap_pop, 1},
    {"missing_arg", (DL_FUNC) &missing_arg, 0},
    {NULL, NULL, 0}
};

void R_init_collections(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
