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
