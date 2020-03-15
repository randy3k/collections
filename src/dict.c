#include "tommyds/tommyhashlin.h"
#include "dict.h"
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
    const char* key;
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


static const char* validate_key(SEXP key) {
    if (!Rf_isVectorAtomic(key) && !Rf_isNull(key)) {
        const char* buf = R_alloc(sizeof(char), 30);
        sprintf((char*) buf, "\xef\x80\x80\x30%p", key);
        return buf;
    } else if (TYPEOF(key) != STRSXP || Rf_length(key) != 1) {
        Rf_error("expect scalar character or non atomic object");
    }
    SEXP c = Rf_asChar(key);
    if (Rf_StringBlank(c) || Rf_isNull(c)) {
        Rf_error("invalid key");
    }
    return Rf_translateCharUTF8(c);
}


static tommy_hashlin* init_hashlin(SEXP self, SEXP ht_xptr) {
    tommy_hashlin* ht;
    ht = malloc(sizeof(tommy_hashlin));
    tommy_hashlin_init(ht);
    R_SetExternalPtrAddr(ht_xptr, ht);
    R_RegisterCFinalizerEx(ht_xptr, &free_ht, TRUE);

    // restore hash table after it has been serialized
    item* s;
    tommy_hash_t hashed_key;
    int i;
    int n = get_int_value(self, "n");
    if (n > 0) {
        const char* key;
        SEXP ks = get_sexp_value(self, "ks");
        SEXP c;
        R_len_t nks = Rf_length(ks);
        for (i = 0; i < nks; i++) {
            c = VECTOR_ELT(ks, i);
            if (Rf_isNull(c)) continue;
            key = validate_key(c);
            hashed_key = tommy_strhash_u32(0, key);
            s = (item*) malloc(sizeof(item));
            s->key = key;
            s->value = i + 1;
            tommy_hashlin_insert(ht, &s->node, s, hashed_key);
        }
    }

    return ht;
}


static int compare(const void* arg, const void* obj) {
    return strcmp(arg, ((item*) obj)->key);
}


static int _dict_index_get(SEXP self, SEXP ht_xptr, const char* key) {
    tommy_hashlin *ht;
    item *s;
    int index;

    ht = R_ExternalPtrAddr(ht_xptr);
    if (ht == NULL) {
        ht = init_hashlin(self, ht_xptr);
    }
    tommy_hash_t hashed_key = tommy_strhash_u32(0, key);
    s = tommy_hashlin_search(ht, compare, key, hashed_key);
    if (s == NULL) {
        index = -1;
    } else {
        index = s->value;
    }
    return index;
}


SEXP dict_index_get(SEXP self, SEXP ht_xptr, SEXP _key) {
    return Rf_ScalarInteger(_dict_index_get(self, ht_xptr, validate_key(_key)));
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


void _dict_index_set(SEXP self, SEXP ht_xptr, const char* key, int index) {
    tommy_hashlin* ht;
    item *s;

    ht = R_ExternalPtrAddr(ht_xptr);
    if (ht == NULL) {
        ht = init_hashlin(self, ht_xptr);
    }
    tommy_hash_t hashed_key = tommy_strhash_u32(0, key);
    s = (item*) malloc(sizeof(item));
    s->key = key;
    s->value = index;
    tommy_hashlin_insert(ht, &s->node, s, hashed_key);
}


SEXP dict_set(SEXP self, SEXP ht_xptr, SEXP _key, SEXP value) {
    const char* key = validate_key(_key);
    int idx = _dict_index_get(self, ht_xptr, key);
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
        _dict_index_set(self, ht_xptr, key, index);

        SEXP ks = PROTECT(get_sexp_value(self, "ks"));
        if (TYPEOF(_key) == STRSXP) {
            SET_VECTOR_ELT(ks, index - 1, Rf_ScalarString(Rf_mkCharCE(key, CE_UTF8)));
        } else  {
            SET_VECTOR_ELT(ks, index - 1, _key);
        }
        UNPROTECT(1);
    } else {
        index = idx;
    }
    SEXP vs = PROTECT(get_sexp_value(self, "vs"));
    SET_VECTOR_ELT(vs, index - 1, value);
    UNPROTECT(1);
    return Rf_ScalarInteger(idx);
}


SEXP dict_remove(SEXP self, SEXP ht_xptr, SEXP _key) {
    tommy_hashlin *ht;
    item *s;
    const char* key;
    int index;

    ht = R_ExternalPtrAddr(ht_xptr);
    if (ht == NULL) {
        ht = init_hashlin(self, ht_xptr);
    }
    key = validate_key(_key);
    tommy_hash_t hashed_key = tommy_strhash_u32(0, key);
    s = tommy_hashlin_remove(ht, compare, key, hashed_key);
    if (s == NULL) {
        Rf_error("key not found");
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
