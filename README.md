## High-performance container datatypes for R



## Related R packages

[dequer](https://CRAN.R-project.org/package=dequer) implements double ended queues and it supports arbitary R objects. However, it uses `R_PreserveObject` and `R_ReleaseObject` heavily which could be an issue for long queues.

[datastructures](https://CRAN.R-project.org/package=datastructures) uses 'Boost' and 'STL' data types to implement queues and hashmaps. It is very efficient but it only supports atomic vectors.

[liqueueR](https://CRAN.R-project.org/package=liqueueR) implements queues using R lists, so efficiency is a concern.

[hash](https://CRAN.R-project.org/package=hash) uses  `new.env( hash = TRUE)` to create hash tables.

[hashmap](https://CRAN.R-project.org/package=hashmap) provides fast hashmap for atomic vectors.
