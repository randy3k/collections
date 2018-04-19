#' @export
OrderedDict <- R6::R6Class("OrderedDict",
    inherit = Dict,
    private = list(
        q = NULL
    ),
    public = list(
        initialize = function() {
            private$q <- Deque$new()
        },
        set = function(key, value) {
            private$q$push(key)
            super$set(key, value)
        },
        remove = function(key) {
            super$remove(key)
            private$q$remove(key)
        },
        keys = function() {
            private$q$as_list()
        },
        values = function() {
            ret <- list()
            i <- 0
            for (key in self$keys()) {
                i <- i + 1
                ret[[i]] <- self$get(key)
            }
            ret
        }
    )
)
