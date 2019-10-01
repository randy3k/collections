
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
‘Boost’ and ‘STL’ data types to implement queues and hashmaps. For some
reasons, it is often slow as shown in the benchmark.

[liqueueR](https://CRAN.R-project.org/package=liqueueR) implements
queues in pure R code.

[hash](https://CRAN.R-project.org/package=hash) uses R environment to 
create hash tables. As indicated by the author of 
[fastmap](https://CRAN.R-project.org/package=fastmap),
a small amount of memory may be leaked.

[hashmap](https://CRAN.R-project.org/package=hashmap) provides
hashmap for atomic vectors using boost library.

[fastmap](https://CRAN.R-project.org/package=fastmap) provides hashmap
which doesn not leak memory. The current implementation of collection::Dict
is actually inspired by it. However, a more
efficient hash table library [tommy](https://github.com/amadvance/tommyds/)
is used.
