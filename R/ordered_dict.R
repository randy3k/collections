#' @title Ordered Dictionary
#' @description
#' `ordered_dict` creates an ordered directory.
#' `OrderedDict` is deprecated and will be removed from future releases.
#' @param items a list of items
#' @param keys a list of keys, use \code{names(items)} if \code{NULL}
#' @details
#' Following methods are exposed:
#' \preformatted{
#' .$set(key, value)
#' .$get(key, default)
#' .$remove(key)
#' .$pop(key, default)
#' .$popitem(last = TRUE)
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
#' * `d`: an ordered dict
#' @examples
#' d <- ordered_dict(list(apple = 5, orange = 10))
#' d$set("banana", 3)
#' d$get("apple")
#' d$as_list()  # the order the item is preserved
#' d$pop("orange")
#' d$as_list()  # "orange" is removed
#' d$set("orange", 3)$set("pear", 7)  # chain methods
#' @seealso [dict]
#' @export
ordered_dict <- function(items = NULL, keys = NULL) {
    ret <- create_ordered_dict()
    ret$initialize(items, keys)
    ret
}


create_ordered_dict <- function() {
    self <- environment()
    ret <- new.env()

    d <- NULL
    q <- NULL

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
        if (d$self$.set(key, value) == -1) {
            q$push(key)
        }
        invisible(ret)
    }
    get <- function(key, default) {
        d$get(key, default)
    }
    remove <- function(key) {
        tryCatch(
            q$remove(key),
            error = function(e) stop("key not found")
        )
        d$remove(key)
        invisible(ret)
    }
    pop <- function(key, default) {
        v <- get(key, default)
        remove(key)
        v
    }
    popitem <- function(last = TRUE) {
        if (last) {
            key <- q$pop()
        } else {
            key <- q$popleft()
        }
        v <- get(key)
        d$remove(key)
        list(key = key, value = v)
    }
    has <- function(key) {
        d$has(key)
    }
    keys <- function() {
        q$as_list()
    }
    values <- function() {
        ret <- vector("list", size())
        keys <- keys()
        for (i in seq_along(keys)) {
            ret[[i]] <- get(keys[[i]])
        }
        ret
    }
    update <- function(t) {
        for (key in t$keys()) {
            set(key, t$get(key))
        }
        self
    }
    clear <- function() {
        d <<- dict()
        q <<- Deque()
        invisible(ret)
    }
    size <- function() d$size()
    as_list <- function() {
        ret <- vector("list", size())
        keys <- keys()
        names(ret) <- keys
        for (i in seq_along(keys)) {
            ret[[i]] <- get(keys[[i]])
        }
        ret
    }
    print <- function() {
        n <- size()
        cat("OrderedDict object with", n, "item(s)\n")
    }

    ret$self <- self
    ret$initialize <- initialize
    ret$set <- set
    ret$get <- get
    ret$remove <- remove
    ret$pop <- pop
    ret$popitem <- popitem
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
