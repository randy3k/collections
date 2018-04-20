#' @export
OrderedDict <- R6::R6Class("OrderedDict",
    private = list(
        e = NULL,
        q = NULL
    ),
    public = list(
        initialize = function(backend = "dict") {
            if (backend == "dict") {
                private$e <- new.env(hash = TRUE)
                private$q <- Deque$new()
                self$set <- function(key, value) {
                    private$q$push(key)
                    assign(key, value, envir = private$e)
                }
                self$get <- function(key, default = NULL) {
                    private$e[[key]]
                }
                self$remove <- function(key) {
                    .Internal(remove(key, private$e, FALSE))
                    private$q$remove(key)
                }
                self$keys <- function() {
                    private$q$as_list()
                }
                self$size <- function() length(ls(private$e))
                self$as_list <- function() {
                    ret <- list()
                    for (key in self$keys()) {
                        ret[[key]] <- self$get(key)
                    }
                    ret
                }
            } else if (backend == "array") {
                private$e <- list()
                self$set <- function(key, value) {
                    private$e[[key]] <- value
                }
                self$get <- function(key, default = NULL) {
                    private$e[[key]]
                }
                self$remove <- function(key) {
                    private$e <- private$e[self$keys() != key]
                }
                self$keys <- function() {
                    names(private$e)
                }
                self$size <- function() length(private$e)
                self$as_list <- function() private$e
            } else {
                stop("unexpected backend")
            }
        },
        set = NULL,
        get = NULL,
        remove = NULL,
        pop = function(key, default = NULL) {
            v <- self$get(key, default)
            self$remove(key)
            v
        },
        has = function(key) {
            key %in% self$keys()
        },
        keys = NULL,
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
        size = NULL,
        as_list = NULL
    )
)
