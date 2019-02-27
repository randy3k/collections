#' @title Stack
#' @description
#' The `Stack` class creates a stack with pairlist backend.
#' @section Usage:
#' \preformatted{
#' Stack$new()
#' Stack$push(item)
#' Stack$pop()
#' Stack$peek()
#' Stack$clear()
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
        initialize = function() {
            self$clear()
        },
        push = function(item) {
            invisible(.Call("stack_push", PACKAGE = "collections", private, item))
        },
        pop = function() {
            .Call("stack_pop", PACKAGE = "collections", private)
        },
        peek = function() {
            if (is.null(private$q)) stop("stack is empty")
            .Call("pairlist_car", PACKAGE = "collections", private$q)
        },
        clear = function() {
            private$q <- NULL
        },
        size = function() length(private$q),
        as_list = function() as.list(private$q)
    )
)

#' @title Stack (list based)
#' @description
#' The `StackL` class returns a stack with list backend.
#' Pure R implementation, mainly for benchmark.
#' @section Usage:
#' \preformatted{
#' StackL$new()
#' StackL$push(item)
#' StackL$pop()
#' StackL$peek()
#' StackL$clear()
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
        q = NULL,
        n = NULL
    ),
    public = list(
        initialize = function() {
            self$clear()
        },
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
        peek = function() {
            if (private$n == 0) stop("stack is empty")
            private$q[[1]]
        },
        clear = function() {
            private$q <- list()
            private$n <- 0
        },
        size = function() private$n,
        as_list = function() private$q
    )
)
