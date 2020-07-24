#' @title Dictionary
#' @description
#' `dict` creates an ordinary (unordered) dictionary (a.k.a. hash).
#' @param items a list of items
#' @param keys a list of keys, use \code{names(items)} if \code{NULL}
#' @details
#' Following methods are exposed:
#' \preformatted{
#' .$set(key, value)
#' .$get(key, default)
#' .$remove(key, silent = FALSE)
#' .$pop(key, default)
#' .$has(key)
#' .$keys()
#' .$values()
#' .$update(d)
#' .$clear()
#' .$size()
#' .$as_list()
#' .$print()
#' }
#' * `key`: a scalar character, an atomic vector, an enviroment or a function
#' * `value`: any R object, value of the item
#' * `default`: optional, the default value of an item if the key is not found
#' * `d`: a dict object
#' @examples
#' d <- dict(list(apple = 5, orange = 10))
#' d$set("banana", 3)
#' d$get("apple")
#' d$as_list()  # unordered
#' d$pop("orange")
#' d$as_list()  # "orange" is removed
#' d$set("orange", 3)$set("pear", 7)  # chain methods
#'
#' # vector indexing
#' d$set(c(1L, 2L), 3)$set(LETTERS, 26)
#' d$get(c(1L, 2L))  # 3
#' d$get(LETTERS)  # 26
#'
#' # object indexing
#' e <- new.env()
#' d$set(sum, 1)$set(e, 2)
#' d$get(sum)  # 1
#' d$get(e)  # 2
#' @seealso [ordered_dict]
#' @export
dict <- function(items = NULL, keys = NULL) {
    self <- environment()
    .__class__ <- "dict"

    n <- NULL
    m <- NULL
    holes <- NULL
    nholes <- NULL
    vs <- NULL
    ks <- NULL
    ht_xptr <- NULL
    # we will define the keys function
    .keys <- keys

    initialize <- function(items, keys) {
        clear()
        if (is.null(keys)) {
            keys <- names(items)
            for (i in seq_along(items)) {
                set(keys[i], items[[i]])
            }
        } else if (is.character(keys)) {
            for (i in seq_along(items)) {
                set(keys[i], items[[i]])
            }
        } else {
            if (length(items) != length(keys)) stop("items and keys should have the same length")
            for (i in seq_along(items)) {
                set(keys[[i]], items[[i]])
            }
        }
    }
    set <- function(key, value) {
        .Call(C_dict_set, self, key, value)
        invisible(self)
    }
    get <- function(key, default) {
        .Call(C_dict_get, self, key)
    }
    remove <- function(key, silent = FALSE) {
        .Call(C_dict_remove, self, key, silent)
        invisible(self)
    }
    pop <- function(key, default) {
        # TODO: get and remove in one shot
        v <- get(key, default)
        .Call(C_dict_remove, self, key, TRUE)
        v
    }
    has <- function(key) {
        .Call(C_dict_has, self, key)
    }
    keys <- function() {
        .Call(C_dict_keys, self)
    }
    values <- function() {
        .Call(C_dict_values, self)
    }
    update <- function(d) {
        for (key in d$keys()) {
            set(key, d$get(key))
        }
        invisible(self)
    }
    clear <- function() {
        holes <<- stack()
        .Call(C_dict_clear, self)
        invisible(self)
    }
    size <- function() n
    as_list <- function() {
        v <- values()
        names(v) <- keys()
        v
    }
    print <- function() {
        n <- size()
        cat("dict object with", n, "item(s)\n")
    }

    initialize(items, .keys)
    items <- NULL
    .keys <- NULL
    self
}
