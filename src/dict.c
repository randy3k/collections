#include "tommyds/tommyhashlin.h"
#include "dict.h"
#include "xxh.h"
#include "utils.h"

#define INITIAL_SIZE 16
#define GROW_FACTOR 1.5
#define SHRINK_FACTOR 0.15

#if !defined(static_inline)
#if defined(_MSC_VER) || defined(__GNUC__)
#define static_inline static __inline
#else
#define static_inline static
#endif
#endif


typedef struct item {
    SEXP key;
    int value;
    tommy_node node;
} item;


static void free_ht(SEXP ht_xptr) {
    tommy_hashlin* ht = R_ExternalPtrAddr(ht_xptr);

    if (ht) {
        tommy_hashlin_foreach(ht, free);
        tommy_hashlin_done(ht);
        free(ht);
    }
}


static_inline int holes_pop(SEXP self) {
    SEXP holes = PROTECT(get_sexp_value(self, "holes"));
    SEXP pop = PROTECT(get_sexp_value(holes, "pop"));
    SEXP l = PROTECT(Rf_lang1(pop));
    int n = Rf_asInteger(Rf_eval(l, holes));
    UNPROTECT(3);
    return n;
}


static_inline void holes_push(SEXP self, int index) {
    SEXP holes = PROTECT(get_sexp_value(self, "holes"));
    SEXP push = PROTECT(get_sexp_value(holes, "push"));
    SEXP x = PROTECT(Rf_ScalarInteger(index));
    SEXP l = PROTECT(Rf_lang2(push, x));
    Rf_eval(l, holes);
    UNPROTECT(4);
}


static_inline void holes_clear(SEXP self) {
    SEXP holes = PROTECT(get_sexp_value(self, "holes"));
    SEXP clear = PROTECT(get_sexp_value(holes, "clear"));
    SEXP l = PROTECT(Rf_lang1(clear));
    Rf_eval(l, holes);
    UNPROTECT(3);
}


tommy_hash_t key_to_u64(SEXP key) {

    if (is_hashable(key)) {
        return xxh_digest(key);
    }

    if (Rf_isEnvironment(key)) {
        static char ch[50];
        snprintf(ch, 50, "<environment: %p>", (void*) key);
        return XXH3_64bits(ch, strlen(ch));
    }

    if (Rf_isFunction(key)) {
        SEXP key2 = PROTECT(Rf_shallow_duplicate(key));
        // avoid R_Serialize serilizing the closure environment and attributes
        SET_CLOENV(key2, R_NilValue);
        SET_ATTRIB(key2, R_NilValue);
        tommy_hash_t h = xxh_serialized_digest(key2);
        UNPROTECT(1);
        return h;
    }

    Rf_error("key is not hashable");
}


SEXP dict_hash(SEXP key) {
    tommy_hash_t h = key_to_u64(key);
    char* p = R_alloc(17, sizeof(char));
    char* c = (char*) &h;
    for(int j = 0; j < 8; j++) {
        sprintf(p + 2*j, "%02x", c[j]);
    }
    p[16] = 0;
    return Rf_mkString(p);
}


static tommy_hashlin* init_hashlin(SEXP self, SEXP ht_xptr) {
    tommy_hashlin* ht;
    ht = malloc(sizeof(tommy_hashlin));
    tommy_hashlin_init(ht);
    R_SetExternalPtrAddr(ht_xptr, ht);
    R_RegisterCFinalizerEx(ht_xptr, &free_ht, TRUE);

    // restore hash table after it has been serialized
    item* s;
    tommy_hash_t h;
    int i;
    int n = get_int_value(self, "n");
    if (n > 0) {
        SEXP ks = PROTECT(get_sexp_value(self, "ks"));
        SEXP c;
        R_len_t nks = Rf_length(ks);
        for (i = 0; i < nks; i++) {
            c = VECTOR_ELT(ks, i);
            if (Rf_isNull(c)) continue;
            h = key_to_u64(c);
            s = (item*) malloc(sizeof(item));
            s->key = c;
            s->value = i + 1;
            tommy_hashlin_insert(ht, &s->node, s, h);
        }
        UNPROTECT(1);
    }
    return ht;
}


static int compare(const void* arg, const void* obj) {
    // return 0 if match
    return !R_compute_identical((SEXP) arg, (SEXP) ((item*) obj)->key, 16);
}


static int _dict_index_get(SEXP self, SEXP ht_xptr, SEXP _key, tommy_hash_t h) {
    tommy_hashlin *ht;
    item *s;
    int index;

    PROTECT(ht_xptr);
    ht = R_ExternalPtrAddr(ht_xptr);
    if (ht == NULL) {
        ht = init_hashlin(self, ht_xptr);
    }
    s = tommy_hashlin_search(ht, compare, _key, h);
    if (s == NULL) {
        index = -1;
    } else {
        index = s->value;
    }
    UNPROTECT(1);
    return index;
}


SEXP dict_get(SEXP self, SEXP _key) {
    SEXP ht_xptr = PROTECT(get_sexp_value(self, "ht_xptr"));
    tommy_hash_t h = key_to_u64(_key);
    int index = _dict_index_get(self, ht_xptr, _key, h);
    UNPROTECT(1);
    if (index <= 0) {
        SEXP fn = r_current_frame();
        if (r_is_missing(fn, "default")) {
            Rf_error("key not found");
        } else {
            SEXP _default = PROTECT(Rf_findVar(Rf_install("default"), fn));
            _default = Rf_eval(_default, PRENV(_default));
            UNPROTECT(1);
            return _default;
        }
    }
    SEXP vs = get_sexp_value(self, "vs");
    return VECTOR_ELT(vs, index - 1);
}


static void grow(SEXP self, int m) {
    SEXP ks = PROTECT(get_sexp_value(self, "ks"));
    SEXP vs = PROTECT(get_sexp_value(self, "vs"));
    SEXP ks2 = PROTECT(Rf_allocVector(VECSXP, m));
    SEXP vs2 = PROTECT(Rf_allocVector(VECSXP, m));
    int i;
    SEXP c;
    R_len_t nks = Rf_length(ks);
    for (i = 0; i < nks; i++) {
        c = VECTOR_ELT(ks, i);
        if (Rf_isNull(c)) continue;
        SET_VECTOR_ELT(ks2, i, VECTOR_ELT(ks, i));
        SET_VECTOR_ELT(vs2, i, VECTOR_ELT(vs, i));
    }
    for(i = nks; i < m; i++) {
        SET_VECTOR_ELT(ks2, i, R_NilValue);
        SET_VECTOR_ELT(vs2, i, R_NilValue);
    }
    set_sexp_value(self, "ks", ks2);
    set_sexp_value(self, "vs", vs2);
    UNPROTECT(4);
}


static void shrink(SEXP self, int m) {
    SEXP ks = PROTECT(get_sexp_value(self, "ks"));
    SEXP vs = PROTECT(get_sexp_value(self, "vs"));
    SEXP ks2 = PROTECT(Rf_allocVector(VECSXP, m));
    SEXP vs2 = PROTECT(Rf_allocVector(VECSXP, m));
    int i;
    SEXP c;
    R_len_t nks = Rf_length(ks);
    int j = 0;
    for (i = 0; i < nks; i++) {
        c = VECTOR_ELT(ks, i);
        if (Rf_isNull(c)) continue;
        SET_VECTOR_ELT(ks2, j, VECTOR_ELT(ks, i));
        SET_VECTOR_ELT(vs2, j, VECTOR_ELT(vs, i));
        j++;
    }
    for(i = j; i < m; i++) {
        SET_VECTOR_ELT(ks2, i, R_NilValue);
        SET_VECTOR_ELT(vs2, i, R_NilValue);
    }
    set_sexp_value(self, "ks", ks2);
    set_sexp_value(self, "vs", vs2);
    UNPROTECT(4);
}


void _dict_index_set(SEXP self, SEXP ht_xptr, SEXP _key, tommy_hash_t h, int index) {
    tommy_hashlin* ht;
    item *s;

    ht = R_ExternalPtrAddr(ht_xptr);
    if (ht == NULL) {
        ht = init_hashlin(self, ht_xptr);
    }
    s = (item*) malloc(sizeof(item));
    s->key = _key;
    s->value = index;
    tommy_hashlin_insert(ht, &s->node, s, h);
}


SEXP dict_set(SEXP self, SEXP _key, SEXP value) {
    SEXP ht_xptr = PROTECT(get_sexp_value(self, "ht_xptr"));
    tommy_hash_t h = key_to_u64(_key);
    int idx = _dict_index_get(self, ht_xptr, _key, h);
    int index;

    if (idx == -1) {
        int nholes = get_int_value(self, "nholes");
        if (nholes > 0) {
            add_int_value(self, "nholes", -1);
            add_int_value(self, "n", 1);
            index = holes_pop(self);
        } else {
            index = add_int_value(self, "n", 1);
        }
        int m = get_int_value(self, "m");
        if (index > m) {
            int m2 = (int) ceil(GROW_FACTOR * m);
            grow(self, m2);
            set_int_value(self, "m", m2);
        }
        _dict_index_set(self, ht_xptr, _key, h, index);

        SEXP ks = PROTECT(get_sexp_value(self, "ks"));
        SET_VECTOR_ELT(ks, index - 1, _key);
        UNPROTECT(1);
    } else {
        index = idx;
    }
    SEXP vs = PROTECT(get_sexp_value(self, "vs"));
    SET_VECTOR_ELT(vs, index - 1, value);
    UNPROTECT(2);
    return Rf_ScalarInteger(idx);
}


SEXP dict_remove(SEXP self, SEXP _key, SEXP _silent) {
    tommy_hashlin *ht;
    item *s;
    int index;
    int silent = Rf_asInteger(_silent);

    SEXP ht_xptr = PROTECT(get_sexp_value(self, "ht_xptr"));
    ht = R_ExternalPtrAddr(ht_xptr);
    if (ht == NULL) {
        ht = init_hashlin(self, ht_xptr);
    }
    UNPROTECT(1);
    tommy_hash_t h = key_to_u64(_key);
    s = tommy_hashlin_remove(ht, compare, _key, h);
    if (s == NULL) {
        if (silent) {
            return R_NilValue;
        } else {
            Rf_error("key not found");
        }
    }

    index = s->value;
    free(s);

    int n = add_int_value(self, "n", -1);
    SEXP ks = PROTECT(get_sexp_value(self, "ks"));
    SEXP vs = PROTECT(get_sexp_value(self, "vs"));
    SET_VECTOR_ELT(ks, index - 1, R_NilValue);
    SET_VECTOR_ELT(vs, index - 1, R_NilValue);
    UNPROTECT(2);
    holes_push(self, index);
    add_int_value(self, "nholes", 1);
    int m = get_int_value(self, "m");
    int m2 = ceil(m * SHRINK_FACTOR);
    if (n < m2 && m2 > INITIAL_SIZE) {
        shrink(self, m2);
        set_int_value(self, "m", m2);
        holes_clear(self);
        set_int_value(self, "nholes", 0);
        set_sexp_value(self, "ht_xptr", R_MakeExternalPtr(NULL, R_NilValue, R_NilValue));
    }
    return R_NilValue;
}


SEXP dict_has(SEXP self, SEXP _key) {
    SEXP ht_xptr = PROTECT(get_sexp_value(self, "ht_xptr"));
    tommy_hash_t h = key_to_u64(_key);
    int index = _dict_index_get(self, ht_xptr, _key, h);
    UNPROTECT(1);
    return Rf_ScalarLogical(index >= 1);
}


SEXP dict_keys(SEXP self) {
    SEXP ks = PROTECT(get_sexp_value(self, "ks"));
    int n = get_int_value(self, "n");
    SEXP keys = PROTECT(Rf_allocVector(VECSXP, n));
    SEXP key;
    int i, j;
    j = 0;
    for (i = 0; i < Rf_length(ks); i++) {
        key = VECTOR_ELT(ks, i);
        if (!Rf_isNull(key)) {
            SET_VECTOR_ELT(keys, j, key);
            j++;
        }
    }
    UNPROTECT(2);
    return keys;
}


SEXP dict_values(SEXP self) {
    SEXP ks = PROTECT(get_sexp_value(self, "ks"));
    SEXP vs = PROTECT(get_sexp_value(self, "vs"));
    int n = get_int_value(self, "n");
    SEXP values = PROTECT(Rf_allocVector(VECSXP, n));
    SEXP key;
    int i, j;
    j = 0;
    for (i = 0; i < Rf_length(ks); i++) {
        key = VECTOR_ELT(ks, i);
        if (!Rf_isNull(key)) {
            SET_VECTOR_ELT(values, j, VECTOR_ELT(vs, i));
            j++;
        }
    }
    UNPROTECT(3);
    return values;
}


SEXP dict_clear(SEXP self) {
    set_int_value(self, "n", 0);
    set_int_value(self, "m", INITIAL_SIZE);
    SEXP vs = PROTECT(Rf_allocVector(VECSXP, INITIAL_SIZE));
    SEXP ks = PROTECT(Rf_allocVector(VECSXP, INITIAL_SIZE));
    set_sexp_value(self, "vs", vs);
    set_sexp_value(self, "ks", ks);
    SEXP xptr = PROTECT(R_MakeExternalPtr(NULL, R_NilValue, R_NilValue));
    set_sexp_value(self, "ht_xptr", xptr);
    set_int_value(self, "nholes", 0);
    UNPROTECT(3);
    return R_NilValue;
}
