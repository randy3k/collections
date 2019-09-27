# helper to identify if default is missing
missing_arg <- function() .Call("missing_arg", PACKAGE = "collections")


#' @title Dictionary
#' @description
#' The `Dict` function creates an ordinary (unordered) dictionary (a.k.a. hash).
#' @section Usage:
#' \preformatted{
#' Dict(items = NULL)
#' .$set(key, value)
#' .$get(key, default)
#' .$remove(key)
#' .$pop(key, default)
#' .$has(key)
#' .$keys()
#' .$values()
#' .$update(d)
#' .$clear()
#' .$size()
#' .$as_list()
#' }
#' @section Usage:
#' * `items`: initialization list
#' * `key`: any R object, key of the item
#' * `value`: any R object, value of the item
#' * `default`: optional, the default value of an item if the key is not found
#' @examples
#' d <- Dict(list(apple = 5, orange = 10))
#' d$set("banana", 3)
#' d$get("apple")
#' d$as_list()  # unordered
#' d$pop("orange")
#' d$as_list()  # "orange" is removed
#' d$set("orange", 3)$set("pear", 7)  # chain methods
#' @seealso [OrderedDict] and [OrderedDictL]
#' @export
Dict <- function(...) {
    self <- environment()
    e <- NULL

    initialize <- function(items = NULL) {
        clear()
        for (argname in names(items)) {
            set(argname, items[[argname]])
        }
    }
    set <- function(key, value) {
        assign(key, value, envir = e)
        invisible(self)
    }
    get <- function(key, default = missing_arg()) {
        .Call("dict_get", PACKAGE = "collections", e, key, default)
    }
    remove <- function(key) {
        .Internal(remove(key, e, FALSE))
        invisible(self)
    }
    pop <- function(key, default = missing_arg()) {
        v <- get(key, default)
        remove(key)
        v
    }
    has <- function(key) {
        key %in% ls(e)
    }
    keys <- function() {
        ls(e)
    }
    values <- function() {
        ret <- as_list()
        names(ret) <- NULL
        ret
    }
    update <- function(d) {
        for (key in d$keys()) {
            set(key, d$get(key))
        }
        self
    }
    clear <- function() {
        e <<- new.env(hash = TRUE)
        invisible(self)
    }
    size <- function() length(ls(e))
    as_list <- function() as.list(e)

    initialize(...)
    class(self) <- "Dict"
    self
}

#' @method print Dict
#' @export
print.Dict <- function(self) {
    n <- self$size()
    cat("Dict object with", n, "item(s)\n")
}
