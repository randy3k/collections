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

#' @export
StackL <- R6::R6Class("StackL",
    cloneable = FALSE,
    private = list(
        q = list(),
        n = 0
    ),
    public = list(
        push = function(item) {
            private$q <- c(item, private$q)
            private$n <- private$n + 1
            invisible(item)
        },
        pop = function() {
            if (private$n == 0) stop("stack is empty")
            v <- private$q[[1]]
            private$q <- private$q[-1]
            private$n <- private$n - 1
            v
        },
        size = function() private$n,
        as_list = function() private$q
    )
)
