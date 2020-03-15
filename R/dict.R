#' @title Dictionary
#' @description
#' The `Dict` function creates an ordinary (unordered) dictionary (a.k.a. hash).
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
#' @examples
#' d <- Dict(list(apple = 5, orange = 10))
#' d$set("banana", 3)
#' d$get("apple")
#' d$as_list()  # unordered
#' d$pop("orange")
#' d$as_list()  # "orange" is removed
#' d$set("orange", 3)$set("pear", 7)  # chain methods
#' @seealso [OrderedDict] and [OrderedDictL]
#' @importFrom xptr null_xptr
#' @export
Dict <- function(items = NULL, keys = NULL) {
    self <- environment()

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
    # we will define the keys function
    keys0 <- keys

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
    .get_index <- function(key) {
        .Call(C_dict_index_get, self, ht_xptr, key)
    }
    .set <- function(key, value) {
        .Call(C_dict_set, self, ht_xptr, key, value)
    }
    set <- function(key, value) {
        .set(key, value)
        invisible(self)
    }
    get <- function(key, default) {
        index <- .get_index(key)
        if (index > 0L) {
            vs[[index]]
        } else if (missing(default)) {
            stop("key not found")
        } else {
            default
        }
    }
    remove <- function(key) {
        .Call(C_dict_remove, self, ht_xptr, key)
        invisible(self)
    }
    pop <- function(key, default) {
        v <- get(key, default)
        remove(key)
        v
    }
    has <- function(key) {
        .get_index(key) != -1
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
        n <<- 0L
        m <<- INITIAL_SIZE
        vs <<- vector("list", INITIAL_SIZE)
        ks <<- vector("list", INITIAL_SIZE)
        # new("externalptr") doesn't work because it returns a static pointer
        ht_xptr <<- null_xptr()
        holes$clear()
        nholes <<- 0L
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
        cat("Dict object with", n, "item(s)\n")
    }

    initialize(items, keys0)
    items <- NULL
    keys0 <- NULL
    self
}



#' @title Dictionary (R implementation)
#' @description
#' The `DictL` function creates an ordinary (unordered) dictionary (a.k.a. hash).
#' Pure R implementation for benchmarking.
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
#' * `key`: scalar character
#' * `value`: any R object, value of the item
#' * `default`: optional, the default value of an item if the key is not found
#' @examples
#' d <- DictL(list(apple = 5, orange = 10))
#' d$set("banana", 3)
#' d$get("apple")
#' d$as_list()  # unordered
#' d$pop("orange")
#' d$as_list()  # "orange" is removed
#' d$set("orange", 3)$set("pear", 7)  # chain methods
#' @seealso [OrderedDict] and [OrderedDictL]
#' @importFrom utils hasName
#' @export
DictL <- function(items = NULL, keys = NULL) {
    self <- environment()
    e <- NULL
    n <- NULL
    keys0 <- keys

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
        had_name <- has(key)
        assign(key, value, envir = e)
        if (!had_name) n <<- n + 1
        invisible(self)
    }
    get <- function(key, default) {
        if (has(key)) {
            e[[key]]
        } else if (missing(default)) {
            stop("key not found")
        } else {
            default
        }
    }
    remove <- function(key) {
        tryCatch(
            .Internal(remove(key, e, FALSE)),
            warning = function(w) stop("key not found")
        )
        n <<- n - 1
        invisible(self)
    }
    pop <- function(key, default) {
        v <- get(key, default)
        remove(key)
        v
    }
    has <- function(key) {
        hasName(e, key)
    }
    keys <- function() {
        as.list(ls(e))
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
        invisible(self)
    }
    clear <- function() {
        e <<- new.env(hash = TRUE)
        n <<- 0
        invisible(self)
    }
    size <- function() n
    as_list <- function() as.list(e)
    print <- function() {
        cat("DictL object with", n, "item(s)\n")
    }

    initialize(items, keys0)
    items <- NULL
    keys0 <- NULL
    self
}
