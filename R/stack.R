#' @title Stack
#' @description
#' The `Stack` function creates a stack.
#' @param items a list of items
#' @details
#' Following methods are exposed:
#' \preformatted{
#' .$push(item)
#' .$pop()
#' .$peek()
#' .$clear()
#' .$size()
#' .$as_list()
#' .$print()
#' }
#' * `item`: any R object
#' @examples
#' s <- Stack()
#' s$push("first")
#' s$push("second")
#' s$pop()  # second
#' s$pop()  # first
#'
#' s <- Stack(list("foo", "bar"))
#' s$push("baz")$push("bla")
#' @seealso [StackL]
#' @export
Stack <- function(items = NULL) {
    self <- environment()
    q <- NULL

    initialize <- function(items = NULL) {
        clear()
        for (i in seq_along(items)) {
            push(items[[i]])
        }
    }
    push <- function(item) {
        .Call(C_stack_push, self, item)
        invisible(self)
    }
    pop <- function() {
        .Call(C_stack_pop, self)
    }
    peek <- function() {
        if (is.null(q)) stop("stack is empty")
        .Call(C_pairlist_car, q)
    }
    clear <- function() {
        q <<- NULL
        invisible(self)
    }
    size <- function() length(q)
    as_list <- function() as.list(q)
    print <- function() {
        n <- size()
        cat("Stack object with", n, "item(s)\n")
    }

    initialize(items)
    items <- NULL
    self
}


#' @title StackL (R implementation)
#' @description
#' The `StackL` function creates a stack.
#' Pure R implementation for benchmarking.
#' @param items a list of items
#' @details
#' Following methods are exposed:
#' \preformatted{
#' .$push(item)
#' .$pop()
#' .$peek()
#' .$clear()
#' .$size()
#' .$as_list()
#' .$print()
#' }
#' * `item`: any R object
#' @examples
#' s <- StackL()
#' s$push("first")
#' s$push("second")
#' s$pop()  # second
#' s$pop()  # first
#'
#' s <- StackL(list("foo", "bar"))
#' s$push("baz")$push("bla")
#' @seealso [Stack]
#' @export
StackL <- function(items = NULL) {
    self <- environment()

    q <- NULL
    n <- NULL

    initialize <- function(items = NULL) {
        clear()
        for (i in seq_along(items)) {
            push(items[[i]])
        }
    }
    push <- function(item) {
        if (is.null(item)) {
            q[n + 1] <<- list(item)
        } else {
            q[[n + 1]] <<- item
        }
        n <<- n + 1
        invisible(self)
    }
    pop <- function() {
        if (n == 0) stop("stack is empty")
        v <- q[[n]]
        q <<- q[-n]
        n <<- n - 1
        v
    }
    peek <- function() {
        if (n == 0) stop("stack is empty")
        q[[n]]
    }
    clear <- function() {
        q <<- list()
        n <<- 0
        invisible(self)
    }
    size <- function() n
    as_list <- function() q
    print <- function() {
        n <- size()
        cat("StackL object with", n, "item(s)\n")
    }

    initialize(items)
    items <- NULL
    self
}
