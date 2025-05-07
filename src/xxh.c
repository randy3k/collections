#include "xxh.h"

int is_hashable(SEXP key) {
    if (Rf_isNull(key)) {
        return 1;
    } else if (Rf_isVectorAtomic(key)) {
        if (!is_hashable(ATTRIB(key))) {
            return 0;
        }
        return 1;
    } else if (TYPEOF(key) == VECSXP) {
        R_xlen_t i;
        R_xlen_t n = Rf_length(key);
        SEXP keyi;
        for (i = 0; i < n; i++) {
            keyi = PROTECT(VECTOR_ELT(key, i));
            if (!is_hashable(keyi)) {
                UNPROTECT(1);
                return 0;
            }
            UNPROTECT(1);
        }
        if (!is_hashable(ATTRIB(key))) {
            return 0;
        }
        return 1;
    } else if (TYPEOF(key) == LISTSXP) {
        SEXP v;
        while (key != R_NilValue) {
            v = CAR(key);
            if (!is_hashable(v)) {
                return 0;
            }
            key = CDR(key);
        }
        if (!is_hashable(ATTRIB(key))) {
            return 0;
        }
        return 1;
    }
    return 0;
}

// much of the following is derived from the fastdigest package but adapt to xxh

static char* buf1;

static void OutChar(R_outpstream_t stream, int c) {
    XXH3_state_t* const xxh_state = (XXH3_state_t* const)stream->data;
    buf1[0] = (char)c;
    XXH3_64bits_update(xxh_state, buf1, 1);
}

static void OutBytes(R_outpstream_t stream, void* buf, int length) {
    XXH3_state_t* const xxh_state = (XXH3_state_t* const)stream->data;
    XXH3_64bits_update(xxh_state, buf, length);
}

XXH64_hash_t xxh_serialized_digest(SEXP x) {
    XXH3_state_t* const xxh_state = XXH3_createState();
    XXH3_64bits_reset(xxh_state);
    struct R_outpstream_st stream;
    R_pstream_format_t type = R_pstream_binary_format;
    int version = 2;

    buf1 = malloc(1);
    R_InitOutPStream(&stream, (R_pstream_data_t)xxh_state, type, version, OutChar, OutBytes, NULL, R_NilValue);

    R_Serialize(x, &stream);

    XXH64_hash_t res = XXH3_64bits_digest(xxh_state);
    XXH3_freeState(xxh_state);
    free(buf1);
    return res;
}

XXH64_hash_t xxh_digest(SEXP x) {
    if (Rf_length(x) >= 0 && Rf_isVectorAtomic(x)) {
        // note: always materialize ALTREP
        char* p;
        if (TYPEOF(x) == STRSXP) {
            if (Rf_length(x) == 1) {
                p = (char*)Rf_translateCharUTF8(Rf_asChar(x));
                return XXH3_64bits(p, strlen(p));
            } else {
                XXH3_state_t* const xxh_state = XXH3_createState();
                XXH3_64bits_reset(xxh_state);
                R_xlen_t n = Rf_length(x);
                for (R_xlen_t i = 0; i < n; i++) {
                    p = (char*)Rf_translateCharUTF8(STRING_ELT(x, i));
                    XXH3_64bits_update(xxh_state, p, strlen(p));
                }
                XXH64_hash_t res = XXH3_64bits_digest(xxh_state);
                XXH3_freeState(xxh_state);
                return res;
            }
        }
        if (TYPEOF(x) == INTSXP) {
            p = (char*)INTEGER(x);
            return XXH3_64bits(p, Rf_length(x) * sizeof(int));
        }
        if (TYPEOF(x) == REALSXP) {
            p = (char*)REAL(x);
            return XXH3_64bits(p, Rf_length(x) * sizeof(double));
        }
        if (TYPEOF(x) == LGLSXP) {
            p = (char*)LOGICAL(x);
            return XXH3_64bits(p, Rf_length(x) * sizeof(int));
        }
        if (TYPEOF(x) == RAWSXP) {
            p = (char*)RAW(x);
            return XXH3_64bits(p, Rf_length(x));
        }
    }

    return xxh_serialized_digest(x);
}
