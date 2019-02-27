# an R implemention of queue
# inspired by https://github.com/wch/qstack/blob/master/R/queue.R

#' @title Queue
#' @description
#' The `Queue` class creates a queue with pairlist backend.
#' @section Usage:
#' \preformatted{
#' Queue$new()
#' Queue$push(item)
#' Queue$pop()
#' Queue$clear()
#' Queue$size()
#' Queue$as_list()
#' }
#' @section Argument:
#' * `item`: any R object
#' @examples
#' q <- Queue$new()
#' q$push("first")
#' q$push("second")
#' q$pop()  # first
#' q$pop()  # second
#' @seealso [QueueL]
#' @export
Queue <- R6::R6Class("Queue",
    cloneable = FALSE,
    private = list(
        q = NULL,
        last = NULL
    ),
    public = list(
        initialize = function() {
            self$clear()
        },
        push = function(item) {
            invisible(.Call("queue_push", PACKAGE = "collections", private, item))
        },
        pop = function() {
            .Call("queue_pop", PACKAGE = "collections", private)
        },
        clear = function() {
            private$q <- NULL
            private$last <- NULL
        },
        size = function() length(private$q),
        as_list = function() as.list(private$q)
    )
)

#' @title Queue (list based)
#' @description
#' The `QueueL` class creates a queue with list backend.
#' Pure R implementation, mainly for benchmark.
#' @section Usage:
#' \preformatted{
#' QueueL$new()
#' QueueL$push(item)
#' QueueL$pop()
#' QueueL$clear()
#' QueueL$size()
#' QueueL$as_list()
#' }
#' @section Argument:
#' * `item`: any R object
#' @examples
#' q <- QueueL$new()
#' q$push("first")
#' q$push("second")
#' q$pop()  # first
#' q$pop()  # second
#' @seealso [Queue]
#' @export
QueueL <- R6::R6Class("QueueL",
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
            private$q[[private$n + 1]] <- item
            private$n <- private$n + 1
            invisible(item)
        },
        pop = function() {
            if (private$n == 0) stop("queue is empty")
            v <- private$q[[1]]
            private$q <- private$q[-1]
            private$n <- private$n - 1
            v
        },
        clear = function() {
            private$q <- list()
            private$n <- 0
        },
        size = function() private$n,
        as_list = function() private$q
    )
)
