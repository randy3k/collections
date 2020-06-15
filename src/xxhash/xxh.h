#ifndef XXH_H__
#define XXH_H__

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define USE_RINTERNALS
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#undef USE_RINTERNALS

#include "xxh3.h"

XXH64_hash_t xxh_digest(SEXP x);


#endif /* end of include guard: XXH_H__ */
