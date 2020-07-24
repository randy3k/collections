#' @title Stack
#' @description
#' `stack` creates a stack.
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
#' s <- stack()
#' s$push("first")
#' s$push("second")
#' s$pop()  # second
#' s$pop()  # first
#'
#' s <- stack(list("foo", "bar"))
#' s$push("baz")$push("bla")
#' @seealso [queue] and [deque]
#' @export
stack <- function(items = NULL) {
    self <- environment()
    .__class__ <- "stack"

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
        cat("stack object with", n, "item(s)\n")
    }

    initialize(items)
    items <- NULL
    self
}
