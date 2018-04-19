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
            if (is.null(private$q)) {
                private$q <- private$last <- as.pairlist(list(item))
            } else {
                private$last <- pairlist_append(private$last, item)
            }
            invisible(item)
        },
        pop = function() {
            if (is.null(private$q)) stop("queue is empty")
            v <- pairlist_car(private$q)
            private$q <- pairlist_cdr(private$q)
            v
        },
        size = function() length(private$q),
        as_list = function() as.list(private$q)
    )
)
