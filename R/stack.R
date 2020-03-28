#' @title Stack
#' @description
#' `stack` creates a stack. `Stack` is deprecated and will be removed from future releases.
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
#' @seealso [queu] and [deque]
#' @export
stack <- function(items = NULL) {
    ret <- create_stack()
    ret$initialize(items)
    ret
}


create_stack <- function() {
    self <- environment()
    ret <- new.env()
    q <- NULL

    initialize <- function(items = NULL) {
        clear()
        for (i in seq_along(items)) {
            push(items[[i]])
        }
    }
    push <- function(item) {
        .Call(C_stack_push, self, item)
        invisible(ret)
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
        invisible(ret)
    }
    size <- function() length(q)
    as_list <- function() as.list(q)
    print <- function() {
        n <- size()
        cat("Stack object with", n, "item(s)\n")
    }

    ret$initialize <- initialize
    ret$push <- push
    ret$pop <- pop
    ret$peek <- peek
    ret$clear <- clear
    ret$size <- size
    ret$as_list <- as_list
    ret$print <- print
    ret
}
