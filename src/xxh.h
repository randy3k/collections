#ifndef XXH_H__
#define XXH_H__

#include "Rversion.h"

#if (R_VERSION < R_Version(3, 5, 0))

# define ALTREP(x) false

#endif

#include <R.h>
#include <Rinternals.h>

#include "xxhash/xxh3.h"

int is_hashable(SEXP key);

XXH64_hash_t xxh_serialized_digest(SEXP x);

XXH64_hash_t xxh_digest(SEXP x);


#endif /* end of include guard: XXH_H__ */
