# an R implemention of queue
# inspired by https://github.com/wch/qstack/blob/master/R/queue.R

#' @export
Queue <- R6::R6Class("Queue",
    cloneable = FALSE,
    private = list(
        q = NULL,
        last = NULL
    ),
    public = list(
        push = function(item) {
            invisible(.Call("queue_push", PACKAGE = "collections", private, item))
        },
        pop = function() {
            .Call("queue_pop", PACKAGE = "collections", private)
        },
        size = function() length(private$q),
        as_list = function() as.list(private$q)
    )
)

#' @export
QueueL <- R6::R6Class("QueueL",
    cloneable = FALSE,
    private = list(
        q = NULL,
        n = 0
    ),
    public = list(
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
        size = function() private$n,
        as_list = function() private$q
    )
)
