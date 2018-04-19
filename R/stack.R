#' @export
Stack <- R6::R6Class("Stack",
    private = list(
        q = NULL
    ),
    public = list(
        push = function(item) {
            if (is.null(private$q)) {
                private$q <- as.pairlist(list(item))
            } else {
                private$q <- pairlist_prepend(private$q, item)
            }
            invisible(item)
        },
        pop = function() {
            v <- pairlist_car(private$q)
            private$q <- pairlist_cdr(private$q)
            v
        },
        size = function() length(private$q),
        as_list = function() as.list(private$q)
    )
)
