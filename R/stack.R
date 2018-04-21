#' @export
Stack <- R6::R6Class("Stack",
    cloneable = FALSE,
    private = list(
        q = NULL
    ),
    public = list(
        push = function(item) {
            invisible(.Call("stack_push", PACKAGE = "collections", private, item))
        },
        pop = function() {
            .Call("stack_pop", PACKAGE = "collections", private)
        },
        size = function() length(private$q),
        as_list = function() as.list(private$q)
    )
)
