#' @export
PriorityQueue <- R6::R6Class("PriorityQueue",
    cloneable = FALSE,
    private = list(
        h = list(),
        n = 0
    ),
    public = list(
        push = function(item, priority = 0) {
            private$h <- heap_push(private$h, private$n, item, priority)
            private$n <- private$n + 1
            invisible(item)
        },
        pop = function() {
            if (private$n == 0) stop("queue is empty")
            v <- heap_pop(private$h, private$n)
            private$n <- private$n - 1
            v
        },
        size = function() private$n,
        as_list = function() lapply(seq_len(private$n), function(i) private$h[[i]][[2]])
    )
)
