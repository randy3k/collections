heapify <- function(h, n) .Call("heapify", PACKAGE = "collections", h, n)

heap_push <- function(h, n, v, p) .Call("heap_push", PACKAGE = "collections", h, n, v, p)

heap_pop <- function(h, n) .Call("heap_pop", PACKAGE = "collections", h, n)
