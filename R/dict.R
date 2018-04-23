#' @export
Dict <- R6::R6Class("Dict",
    cloneable = FALSE,
    private = list(
        e = new.env(hash = TRUE)
    ),
    public = list(
        set = function(key, value) {
            assign(key, value, envir = private$e)
        },
        get = function(key, default = NULL) {
            private$e[[key]]
        },
        remove = function(key) {
            rm(list = key, envir = private$e)
        },
        pop = function(key, default = NULL) {
            v <- self$get(key, default)
            self$remove(key)
            v
        },
        has = function(key) {
            key %in% ls(private$e)
        },
        keys = function() {
            ls(private$e)
        },
        values = function() {
            self$as_list()
        },
        update = function(d) {
            for (key in d$keys()) {
                self$set(key, d$get(key))
            }
            self
        },
        size = function() length(ls(private$e)),
        as_list = function() as.list(private$e)
    )
)
