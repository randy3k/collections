#include "dict.h"

SEXP dict_get(SEXP e, SEXP key, SEXP _default) {
    SEXP v = Rf_findVarInFrame(e, Rf_installChar(STRING_ELT(key, 0)));
    if (v == R_UnboundValue) {
        return _default;
    } else {
        return v;
    }
}
