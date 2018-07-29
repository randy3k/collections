#' @title Dictionary
#' @description
#' The `Dict` class creates an ordinary (unordered) dictionary.
#' The key-value pairs are stored in an R environment.
#' @section Usage:
#' \preformatted{
#' Dict$new()
#' Dict$set(key, value)
#' Dict$get(key, default = NULL)
#' Dict$remove(key)
#' Dict$pop(key, default = NULL)
#' Dict$has(key)
#' Dict$keys()
#' Dict$values()
#' Dict$update(d)
#' Dict$size()
#' Dict$as_list()
#' }
#' @section Usage:
#' * `key`: any R object, key of the item
#' * `value`: any R object, value of the item
#' * `default`: the default value of an item if the key is not found
#' @examples
#' d <- Dict$new()
#' d$set("apple", 5)
#' d$set("orange", 10)
#' d$set("banana", 3)
#' d$get("apple")
#' d$as_list()  # unordered
#' d$pop("orange")
#' d$as_list()  # "orange" is removed
#' @seealso [OrderedDict] and [OrderedDictL]
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
            if (is.null(key)) stop("key cannot be empty")
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
