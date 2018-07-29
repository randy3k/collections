#' @title Stack
#' @description
#' The `Stack` class creates a stack with pairlist backend.
#' It is recommended for long stack
#' @section Usage:
#' \preformatted{
#' Stack$new()
#' Stack$push(item)
#' Stack$pop()
#' Stack$size()
#' Stack$as_list()
#' }
#' @section Argument:
#' * `item`: any R object
#' @examples
#' s <- Stack$new()
#' s$push("first")
#' s$push("second")
#' s$pop()  # second
#' s$pop()  # first
#' @seealso [StackL]
#' @export
Stack <- R6::R6Class("Stack",
    cloneable = FALSE,
    private = list(
        q = NULL
    ),
    public = list(
        push = function(item) {
            invisible(.Call("stack_push", PACKAGE = "collections", private, item))
        },
        pop = function() {
            .Call("stack_pop", PACKAGE = "collections", private)
        },
        size = function() length(private$q),
        as_list = function() as.list(private$q)
    )
)

#' @title Stack (list based)
#' @description
#' The `StackL` class returns a stack with list backend.
#' It is recommended for short stack
#' @section Usage:
#' \preformatted{
#' StackL$new()
#' StackL$push(item)
#' StackL$pop()
#' StackL$size()
#' StackL$as_list()
#' }
#' @section Argument:
#' * `item`: any R object
#' @examples
#' s <- StackL$new()
#' s$push("first")
#' s$push("second")
#' s$pop()  # second
#' s$pop()  # first
#' @seealso [StackL]
#' @export
StackL <- R6::R6Class("StackL",
    cloneable = FALSE,
    private = list(
        q = list(),
        n = 0
    ),
    public = list(
        push = function(item) {
            private$q <- c(item, private$q)
            private$n <- private$n + 1
            invisible(item)
        },
        pop = function() {
            if (private$n == 0) stop("stack is empty")
            v <- private$q[[1]]
            private$q <- private$q[-1]
            private$n <- private$n - 1
            v
        },
        size = function() private$n,
        as_list = function() private$q
    )
)
