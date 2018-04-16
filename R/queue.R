# an R implemention of queue
# inspired by https://github.com/wch/qstack/blob/master/R/queue.R

#' @export
Queue <- R6::R6Class("Queue",
    private = list(
        q = NULL,
        last = NULL
    ),
    public = list(
        put = function(item) {
            if (is.null(private$q)) {
                private$q <- private$last <- as.pairlist(list(item))
            } else {
                private$last <- .Call(
                    "pairlist_append", PACKAGE = "collections", private$last, item)
            }
            invisible(item)
        },
        get = function() {
            v <- .Call("pairlist_car", PACKAGE = "collections", private$q)
            private$q <- .Call("pairlist_cdr", PACKAGE = "collections", private$q)
            v
        },
        size = function() length(private$q),
        as_list = function() as.list(private$q)
    )
)
