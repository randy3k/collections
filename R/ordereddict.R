#' @title Ordered Dictionary
#' @description
#' The `OrderedDict` class creates an ordered dictionary.
#' Keys are stored in a double ended queue [Deque] while items are stored in an R environment.
#' @section Usage:
#' \preformatted{
#' OrderedDict$new()
#' OrderedDict$set(key, value)
#' OrderedDict$get(key, default = NULL)
#' OrderedDict$remove(key)
#' OrderedDict$pop(key, default = NULL)
#' OrderedDict$popitem(last = TRUE, default = NULL)
#' OrderedDict$has(key)
#' OrderedDict$keys()
#' OrderedDict$values()
#' OrderedDict$update(d)
#' OrderedDict$size()
#' OrderedDict$as_list()
#' }
#' @section Argument:
#' * `key`: any R object, key of the item
#' * `value`: any R object, value of the item
#' * `default`: the default value of an item if the key is not found
#' * `d`: an OrderedDict or OrderedDictL
#' @examples
#' d <- OrderedDict$new()
#' d$set("apple", 5)
#' d$set("orange", 10)
#' d$set("banana", 3)
#' d$get("apple")
#' d$as_list()  # the order the item is preserved
#' d$pop("orange")
#' d$as_list()  # "orange" is removed
#' @seealso [Dict] and [OrderedDictL]
#' @export
OrderedDict <- R6::R6Class("OrderedDict",
    cloneable = FALSE,
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
            .Call("dict_get", PACKAGE = "collections", private$e, key, default)
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
        popitem = function(last = TRUE, default = NULL) {
            if (last) {
                key <- private$q$pop()
            } else {
                key <- private$q$popleft()
            }
            v <- self$get(key, default)
            .Internal(remove(key, private$e, FALSE))
            list(key = key, value = v)
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

#' @title Ordered Dictionary (list based)
#' @description
#' The `OrderedDictL` class creates an ordered dictionary.
#' The key-value pairs are stored in an R environment.
#' Pure R implementation, mainly for benchmark.
#' @section Usage:
#' \preformatted{
#' OrderedDictL$new()
#' OrderedDictL$set(key, value)
#' OrderedDictL$get(key, default = NULL)
#' OrderedDictL$remove(key)
#' OrderedDictL$pop(key, default = NULL)
#' OrderedDict$popitem(last = TRUE, default = NULL)
#' OrderedDictL$has(key)
#' OrderedDictL$keys()
#' OrderedDictL$values()
#' OrderedDictL$update(d)
#' OrderedDictL$size()
#' OrderedDictL$as_list()
#' }
#' @section Argument:
#' * `key`: any R object, key of the item
#' * `value`: any R object, value of the item
#' * `default`: the default value of an item if the key is not found
#' * `d`: an OrderedDict or OrderedDictL
#' @examples
#' d <- OrderedDictL$new()
#' d$set("apple", 5)
#' d$set("orange", 10)
#' d$set("banana", 3)
#' d$get("apple")
#' d$as_list()  # the order the item is preserved
#' d$pop("orange")
#' d$as_list()  # "orange" is removed
#' @seealso [Dict] and [OrderedDict]
#' @export
OrderedDictL <- R6::R6Class("OrderedDictL",
    cloneable = FALSE,
    private = list(
        e = list()
    ),
    public = list(
        set = function(key, value) {
            private$e[[key]] <- value
        },
        get = function(key, default = NULL) {
            if (self$has(key)) {
                private$e[[key]]
            } else {
                default
            }
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
        popitem = function(last = TRUE, default = NULL) {
            if (last) {
                keys <- self$keys()
                key <- key[length(keys)]
            } else {
                keys <- self$keys()[1]
            }
            v <- self$get(key, default)
            self$remove(key)
            list(key = key, vlaue = v)
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
