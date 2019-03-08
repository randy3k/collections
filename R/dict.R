# helper to identify if default is missing
missing_arg <- function() .Call("missing_arg", PACKAGE = "collections")


#' @title Dictionary
#' @description
#' The `Dict` class creates an ordinary (unordered) dictionary.
#' The key-value pairs are stored in an R environment.
#' @section Usage:
#' \preformatted{
#' Dict$new(items = NULL)
#' Dict$set(key, value)
#' Dict$get(key, default)
#' Dict$remove(key)
#' Dict$pop(key, default)
#' Dict$has(key)
#' Dict$keys()
#' Dict$values()
#' Dict$update(d)
#' Dict$clear()
#' Dict$size()
#' Dict$as_list()
#' }
#' @section Usage:
#' * `items`: initialization list
#' * `key`: any R object, key of the item
#' * `value`: any R object, value of the item
#' * `default`: optional, the default value of an item if the key is not found
#' @examples
#' d <- Dict$new(list(apple = 5, orange = 10))
#' d$set("banana", 3)
#' d$get("apple")
#' d$as_list()  # unordered
#' d$pop("orange")
#' d$as_list()  # "orange" is removed
#' d$set("orange", 3)$set("pear", 7)  # chain methods
#' @seealso [OrderedDict] and [OrderedDictL]
#' @export
Dict <- R6::R6Class("Dict",
    cloneable = FALSE,
    private = list(
        e = NULL
    ),
    public = list(
        initialize = function(items = NULL) {
            self$clear()
            for (argname in names(items)) {
                self$set(argname, items[[argname]])
            }
        },
        set = function(key, value) {
            assign(key, value, envir = private$e)
            invisible(self)
        },
        get = function(key, default = missing_arg()) {
            .Call("dict_get", PACKAGE = "collections", private$e, key, default)
        },
        remove = function(key) {
            .Internal(remove(key, private$e, FALSE))
            invisible(self)
        },
        pop = function(key, default = missing_arg()) {
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
            ret <- self$as_list()
            names(ret) <- NULL
            ret
        },
        update = function(d) {
            for (key in d$keys()) {
                self$set(key, d$get(key))
            }
            self
        },
        clear = function() {
            private$e <- new.env(hash = TRUE)
        },
        size = function() length(ls(private$e)),
        as_list = function() as.list(private$e),
        print = function() {
            n <- self$size()
            cat("Dict object with", n, "item(s).\n")
        }
    )
)
