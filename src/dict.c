#include "dict.h"

SEXP missing_arg() {
    return R_MissingArg;
}

SEXP dict_get(SEXP e, SEXP key, SEXP _default) {
    SEXP v = Rf_findVarInFrame(e, Rf_installChar(STRING_ELT(key, 0)));
    if (v == R_UnboundValue) {
        if (_default == R_MissingArg) {
            Rf_error("key not found");
        } else {
            return _default;
        }
    } else {
        return v;
    }
}
