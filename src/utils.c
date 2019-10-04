#include "utils.h"


SEXP get_value(SEXP env, const char* name) {
    SEXP x = Rf_findVarInFrame(env, Rf_install(name));
    if (x == R_UnboundValue) {
        Rf_error("variable %s not found", name);
    }
    return x;
}


int get_int_value(SEXP env, const char* name) {
    return Rf_asInteger(get_value(env, name));
}


int add_int_value(SEXP env, const char* name, int v) {
    int x = get_int_value(env, name);
    x = x + v;
    SEXP x_ = PROTECT(Rf_ScalarInteger(x));
    Rf_defineVar(Rf_install(name), x_, env);
    UNPROTECT(1);
    return x;
}
