pairlist_car <- function(x) .Call("pairlist_car", PACKAGE = "collections", x)

pairlist_cdr <- function(x) .Call("pairlist_cdr", PACKAGE = "collections", x)

pairlist_setcar <- function(x, value) .Call("pairlist_setcar", PACKAGE = "collections", x, value)

pairlist_setcdr <- function(x, value) .Call("pairlist_setcdr", PACKAGE = "collections", x, value)

pairlist_append <- function(x, value) .Call("pairlist_append", PACKAGE = "collections", x, value)

pairlist_prepend <- function(x, value) .Call("pairlist_prepend", PACKAGE = "collections", x, value)
