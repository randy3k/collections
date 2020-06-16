#ifndef XXH_H__
#define XXH_H__

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Rversion.h"

#if (R_VERSION < R_Version(3, 5, 0))

# define ALTREP(x) false

#endif

#define USE_RINTERNALS
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#undef USE_RINTERNALS

#include "xxhash/xxh3.h"

XXH64_hash_t xxh_serialized_digest(SEXP x);

XXH64_hash_t xxh_digest(SEXP x);


#endif /* end of include guard: XXH_H__ */
