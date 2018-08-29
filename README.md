
<!-- README.md is generated from README.Rmd. Please edit that file -->

# High Performance Container Data Types

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/collections)](https://cran.r-project.org/package=collections)

Github: <https://github.com/randy3k/collections>

Documentation:
[http://randy3k.github.io/collections](https://randy3k.github.io/collections)

Provides high performance container data types such as Queue, Stack,
Deque, Dict and OrderedDict. Benchmarks
<https://randy3k.github.io/collections/articles/benchmark.html> have
shown that these containers are asymptotically more efficient than those
offered by other packages.

## Related R packages

[dequer](https://CRAN.R-project.org/package=dequer) implements double
ended queues and it supports arbitary R objects. However, it uses
`R_PreserveObject` and `R_ReleaseObject` heavily which could be an issue
for long queues.

[datastructures](https://CRAN.R-project.org/package=datastructures) uses
‘Boost’ and ‘STL’ data types to implement queues and hashmaps.

[liqueueR](https://CRAN.R-project.org/package=liqueueR) implements
queues using R lists.

[hash](https://CRAN.R-project.org/package=hash) uses `new.env( hash =
TRUE)` to create hash tables.

[hashmap](https://CRAN.R-project.org/package=hashmap) provides fast
hashmap for atomic vectors.
