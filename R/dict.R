#' @title Dictionary
#' @description
#' `dict` creates a directory (a.k.a hash map).
#' `Dict` is deprecated and will be removed from future releases.
#' @param items a list of items
#' @param keys a list of keys, use \code{names(items)} if \code{NULL}
#' @details
#' Following methods are exposed:
#' \preformatted{
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
#' .$print()
#' }
#' * `key`: scalar character, environment or function
#' * `value`: any R object, value of the item
#' * `default`: optional, the default value of an item if the key is not found
#' * `d`: a dictionary
#' @examples
#' d <- dict(list(apple = 5, orange = 10))
#' d$set("banana", 3)
#' d$get("apple")
#' d$as_list()  # unordered
#' d$pop("orange")
#' d$as_list()  # "orange" is removed
#' d$set("orange", 3)$set("pear", 7)  # chain methods
#' @seealso [ordered_dict]
#' @importFrom xptr null_xptr
#' @export
dict <- function(items = NULL, keys = NULL) {
    ret <- create_dict()
    ret$initialize(items, keys)
    ret
}


create_dict <- function() {
    self <- environment()
    ret <- list()

    INITIAL_SIZE <- 16L
    GROW_FACTOR <- 1.5
    SHRINK_FACTOR <- 0.15

    n <- NULL
    m <- NULL
    holes <- Stack()
    nholes <- NULL
    vs <- NULL
    ks <- NULL
    ht_xptr <- NULL

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
    .set <- function(key, value) {
        .Call(C_dict_set, self, ht_xptr, key, value)
    }
    set <- function(key, value) {
        .Call(C_dict_set, self, ht_xptr, key, value)
        invisible(ret)
    }
    get <- function(key, default) {
        .Call(C_dict_get, self, ht_xptr, key, if (missing(default)) missing_arg() else default)
    }
    remove <- function(key) {
        .Call(C_dict_remove, self, ht_xptr, key)
        invisible(ret)
    }
    pop <- function(key, default) {
        v <- get(key, default)
        remove(key)
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
        invisible(ret)
    }
    clear <- function() {
        n <<- 0L
        m <<- INITIAL_SIZE
        vs <<- vector("list", INITIAL_SIZE)
        ks <<- vector("list", INITIAL_SIZE)
        # new("externalptr") doesn't work because it returns a static pointer
        ht_xptr <<- null_xptr()
        holes$clear()
        nholes <<- 0L
        invisible(ret)
    }
    size <- function() n
    as_list <- function() {
        v <- values()
        names(v) <- keys()
        v
    }
    print <- function() {
        n <- size()
        cat("Dict object with", n, "item(s)\n")
    }

    ret$self <- self
    ret$initialize <- initialize
    ret$set <- set
    ret$get <- get
    ret$remove <- remove
    ret$pop <- pop
    ret$has <- has
    ret$keys <- keys
    ret$values <- values
    ret$update <- update
    ret$clear <- clear
    ret$size <- size
    ret$as_list <- as_list
    ret$print <- print
    ret
}
