#' @title Stack
#' @description
#' The `Stack` class creates a stack with pairlist backend.
#' @section Usage:
#' \preformatted{
#' Stack$new(items = NULL)
#' Stack$push(item)
#' Stack$pop()
#' Stack$peek()
#' Stack$clear()
#' Stack$size()
#' Stack$as_list()
#' }
#' @section Argument:
#' * `items`: initialization list
#' * `item`: any R object
#' @examples
#' s <- Stack$new()
#' s$push("first")
#' s$push("second")
#' s$pop()  # second
#' s$pop()  # first
#'
#' s <- Stack$new(list("foo", "bar"))
#' s$push("baz")$push("bla")
#' @seealso [StackL]
#' @export
Stack <- R6::R6Class("Stack",
    cloneable = FALSE,
    private = list(
        q = NULL
    ),
    public = list(
        initialize = function(items = NULL) {
            self$clear()
            for (i in seq_along(items)) {
                self$push(items[[i]])
            }
        },
        push = function(item) {
            .Call("stack_push", PACKAGE = "collections", private, item)
            invisible(self)
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
        as_list = function() as.list(private$q),
        print = function() {
            n <- self$size()
            cat("Stack object with", n, "item(s).\n")
        }
    )
)

#' @title Stack (list based)
#' @description
#' The `StackL` class returns a stack with list backend.
#' Pure R implementation, mainly for benchmark.
#' @section Usage:
#' \preformatted{
#' StackL$new(items = NULL)
#' StackL$push(item)
#' StackL$pop()
#' StackL$peek()
#' StackL$clear()
#' StackL$size()
#' StackL$as_list()
#' }
#' @section Argument:
#' * `items`: initialization list
#' * `item`: any R object
#' @examples
#' s <- StackL$new()
#' s$push("first")
#' s$push("second")
#' s$pop()  # second
#' s$pop()  # first
#'
#' s <- StackL$new(list("foo", "bar"))
#' s$push("baz")$push("bla")
#' @seealso [StackL]
#' @export
StackL <- R6::R6Class("StackL",
    cloneable = FALSE,
    private = list(
        q = NULL,
        n = NULL
    ),
    public = list(
        initialize = function(items = NULL) {
            self$clear()
            for (i in seq_along(items)) {
                self$push(items[[i]])
            }
        },
        push = function(item) {
            private$q <- c(item, private$q)
            private$n <- private$n + 1
            invisible(self)
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
        as_list = function() private$q,
        print = function() {
            n <- self$size()
            cat("StackL object with", n, "item(s).\n")
        }
    )
)
