#' @export
OrderedDict <- R6::R6Class("OrderedDict",
    private = list(
        e = new.env(hash = TRUE),
        q = Deque$new()
    ),
    public = list(
        set = function(key, value) {
            private$q$push(key)
            assign(key, value, envir = private$e)
        },
        get = function(key, default = NULL) {
            private$e[[key]]
        },
        remove = function(key) {
            private$q$remove(key)
            .Internal(remove(key, private$e, FALSE))
            invisible(NULL)
        },
        pop = function(key, default = NULL) {
            v <- self$get(key, default)
            self$remove(key)
            v
        },
        has = function(key) {
            key %in% self$keys()
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
        },
        update = function(d) {
            for (key in d$keys()) {
                self$set(key, d$get(key))
            }
            self
        },
        size = function() length(ls(private$e)),
        as_list = function() {
            ret <- list()
            for (key in self$keys()) {
                ret[[key]] <- self$get(key)
            }
            ret
        }
    )
)

#' @export
NaiveOrderedDict <- R6::R6Class("NaiveOrderedDict",
    private = list(
        e = list()
    ),
    public = list(
        set = function(key, value) {
            private$e[[key]] <- value
        },
        get = function(key, default = NULL) {
            private$e[[key]]
        },
        remove = function(key) {
            v <- self$keys() != key
            if (all(v)) stop("value not found")
            private$e <- private$e[v]
            invisible(NULL)
        },
        pop = function(key, default = NULL) {
            v <- self$get(key, default)
            self$remove(key)
            v
        },
        has = function(key) {
            key %in% self$keys()
        },
        keys = function() {
            names(private$e)
        },
        values = function() {
            ret <- list()
            i <- 0
            for (key in self$keys()) {
                i <- i + 1
                ret[[i]] <- self$get(key)
            }
            ret
        },
        update = function(d) {
            for (key in d$keys()) {
                self$set(key, d$get(key))
            }
            self
        },
        size = function() length(private$e),
        as_list = function() private$e
    )
)
