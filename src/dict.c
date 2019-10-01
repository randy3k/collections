#include "tommyds/tommyhashlin.h"
#include "dict.h"
#include "utils.h"


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

static const char* validate_key(SEXP key) {
    if (TYPEOF(key) != STRSXP || Rf_length(key) != 1) {
        Rf_error("expect scalar character");
    }
    SEXP c = Rf_asChar(key);
    if (Rf_StringBlank(c) || c == R_NaString) {
        Rf_error("invalid key");
    }
    return R_CHAR(Rf_asChar(key));
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
        SEXP ks = get_value(self, "ks");
        SEXP c;
        R_len_t nks = Rf_length(ks);
        for (i = 0; i < nks; i++) {
            c = STRING_ELT(ks, i);
            if (c == R_NaString) continue;
            key = R_CHAR(c);
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


SEXP dict_index_get(SEXP self, SEXP ht_xptr, SEXP _key) {
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
    s = tommy_hashlin_search(ht, compare, key, hashed_key);
    if (s == NULL) {
        index = -1;
    } else {
        index = s->value;
    }
    return Rf_ScalarInteger(index);
}


SEXP dict_index_set(SEXP self, SEXP ht_xptr, SEXP _key, SEXP value) {
    tommy_hashlin* ht;
    item *s;
    const char* key;

    ht = R_ExternalPtrAddr(ht_xptr);
    if (ht == NULL) {
        ht = init_hashlin(self, ht_xptr);
    }
    key = validate_key(_key);
    tommy_hash_t hashed_key = tommy_strhash_u32(0, key);
    s = (item*) malloc(sizeof(item));
    s->key = key;
    s->value = Rf_asInteger(value);
    tommy_hashlin_insert(ht, &s->node, s, hashed_key);
    return (R_NilValue);
}


SEXP dict_index_remove(SEXP self, SEXP ht_xptr, SEXP _key) {
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
        index = -1;
    } else {
        index = s->value;
        free(s);
    }
    return Rf_ScalarInteger(index);
}
