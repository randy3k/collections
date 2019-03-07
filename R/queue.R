# an R implemention of queue
# inspired by https://github.com/wch/qstack/blob/master/R/queue.R

#' @title Queue
#' @description
#' The `Queue` class creates a queue with pairlist backend.
#' @section Usage:
#' \preformatted{
#' Queue$new(items = NULL)
#' Queue$push(item)
#' Queue$pop()
#' Queue$peek()
#' Queue$clear()
#' Queue$size()
#' Queue$as_list()
#' }
#' @section Argument:
#' * `items`: initialization list
#' * `item`: any R object
#' @examples
#' q <- Queue$new()
#' q$push("first")
#' q$push("second")
#' q$pop()  # first
#' q$pop()  # second
#'
#' q <- Queue$new(list("foo", "bar"))
#' q$push("baz")$push("bla")
#' @seealso [QueueL]
#' @export
Queue <- R6::R6Class("Queue",
    cloneable = FALSE,
    private = list(
        q = NULL,
        last = NULL
    ),
    public = list(
        initialize = function(items = NULL) {
            self$clear()
            for (i in seq_along(items)) {
                self$push(items[[i]])
            }
        },
        push = function(item) {
            .Call("queue_push", PACKAGE = "collections", private, item)
            invisible(self)
        },
        pop = function() {
            .Call("queue_pop", PACKAGE = "collections", private)
        },
        peek = function() {
            if (is.null(private$q)) stop("queue is empty")
            .Call("pairlist_car", PACKAGE = "collections", private$q)
        },
        clear = function() {
            private$q <- NULL
            private$last <- NULL
        },
        size = function() length(private$q),
        as_list = function() as.list(private$q),
        print = function() {
            n <- self$size()
            cat("Queue object with", n, "item(s).\n")
        }
    )
)

#' @title Queue (list based)
#' @description
#' The `QueueL` class creates a queue with list backend.
#' Pure R implementation, mainly for benchmark.
#' @section Usage:
#' \preformatted{
#' QueueL$new(...)
#' QueueL$push(item)
#' QueueL$pop()
#' QueueL$peek()
#' QueueL$clear()
#' QueueL$size()
#' QueueL$as_list()
#' }
#' @section Argument:
#' * `...`: initialization list
#' * `item`: any R object
#' @examples
#' q <- QueueL$new()
#' q$push("first")
#' q$push("second")
#' q$pop()  # first
#' q$pop()  # second
#'
#' q <- QueueL$new(list("foo", "bar"))
#' q$push("baz")$push("bla")
#' @seealso [Queue]
#' @export
QueueL <- R6::R6Class("QueueL",
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
            private$q[[private$n + 1]] <- item
            private$n <- private$n + 1
            invisible(self)
        },
        pop = function() {
            if (private$n == 0) stop("queue is empty")
            v <- private$q[[1]]
            private$q <- private$q[-1]
            private$n <- private$n - 1
            v
        },
        peek = function() {
            if (private$n == 0) stop("queue is empty")
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
            cat("QueueL object with", n, "item(s).\n")
        }
    )
)
