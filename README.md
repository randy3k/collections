<!-- README.md is generated from README.Rmd. Please edit that file -->

# High Performance Container Data Types

[![Github Action](https://github.com/randy3k/collections/workflows/build/badge.svg?branch=master)](https://github.com/randy3k/collections)
[![codecov](https://codecov.io/gh/randy3k/collections/branch/master/graph/badge.svg)](https://codecov.io/gh/randy3k/collections)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/collections)](https://cran.r-project.org/package=collections)
[![](http://cranlogs.r-pkg.org/badges/grand-total/collections)](https://cran.r-project.org/package=collections)

Documentation: [http://randy3k.github.io/collections](https://randy3k.github.io/collections)

Provides high performance container data types such
as queues, stacks, deques, dicts and ordered dicts. Benchmarks
<https://randy3k.github.io/collections/articles/benchmark.html> have
shown that these containers are asymptotically more efficient than
those offered by other packages.

## Installation

You can install the released version of collections from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("collections")
```

Install the latest development version using

``` r
devtools::install_github("randy3k/collections")
```

## Example

``` r
library(collections, warn.conflicts = FALSE)
```

Queue

``` r
q <- queue()
q$push(1)$push(2)
q$pop()
#> [1] 1
```

Stack

``` r
s <- stack()
s$push(1)$push(2)
s$pop()
#> [1] 2
```

Deque

``` r
dq <- deque()
dq$push(1)$pushleft(2)
dq$pop()
#> [1] 1
```

Priority Queue

``` r
pq <- priority_queue()
pq$push("not_urgent")
pq$push("urgent", priority = 2)
pq$push("not_as_urgent", priority = 1)
pq$pop()
#> [1] "urgent"
pq$pop()
#> [1] "not_as_urgent"
pq$pop()
#> [1] "not_urgent"
```

Dictionary. Comparing to R envrionments, `dict()` does not [leak memory](https://r-lib.github.io/fastmap/#memory-leak-examples) and supports hashing functions and environments.

``` r
d <- dict()
e <- new.env()
d$set(e, 1)$set(sum, 2)
d$get(e)
#> [1] 1
```

Ordered Dictionary

``` r
d <- ordered_dict()
d$set("b", 1)$set("a", 2)
d$as_list()
#> $b
#> [1] 1
#> 
#> $a
#> [1] 2
```
