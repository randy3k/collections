#' @export
PriorityQueue <- R6::R6Class("PriorityQueue",
    cloneable = FALSE,
    private = list(
        h = list(),
        n = 0
    ),
    public = list(
        push = function(item, priority = 0) {
            invisible(.Call("heap_push", PACKAGE = "collections", private, item, priority))
        },
        pop = function() {
            .Call("heap_pop", PACKAGE = "collections", private)
        },
        size = function() private$n,
        as_list = function() lapply(seq_len(private$n), function(i) private$h[[i]][[2]])
    )
)
