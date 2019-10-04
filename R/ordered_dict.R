#' @title Ordered Dictionary
#' @description
#' The `OrderedDict` function creates an ordered dictionary.
#' @param items a list of items
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
#' * `key`: any R object, key of the item
#' * `value`: any R object, value of the item
#' * `default`: optinal, the default value of an item if the key is not found
#' * `d`: an OrderedDict or OrderedDictL
#' @examples
#' d <- OrderedDict(list(apple = 5, orange = 10))
#' d$set("banana", 3)
#' d$get("apple")
#' d$as_list()  # the order the item is preserved
#' d$pop("orange")
#' d$as_list()  # "orange" is removed
#' d$set("orange", 3)$set("pear", 7)  # chain methods
#' @seealso [Dict] and [OrderedDictL]
#' @export
OrderedDict <- function(items = NULL) {
    self <- environment()
    d <- NULL
    q <- NULL

    initialize <- function(items = NULL) {
        clear()
        for (argname in names(items)) {
            set(argname, items[[argname]])
        }
    }
    set <- function(key, value) {
        if (d$.set(key, value) == -1) {
            q$push(key)
        }
        invisible(self)
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
        invisible(self)
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
        unlist(q$as_list())
    }
    values <- function() {
        ret <- vector("list", size())
        keys <- keys()
        for (i in seq_along(keys)) {
            ret[[i]] <- get(keys[i])
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
        d <<- Dict()
        q <<- Deque()
        invisible(self)
    }
    size <- function() d$size()
    as_list <- function() {
        ret <- vector("list", size())
        keys <- keys()
        names(ret) <- keys
        for (i in seq_along(keys)) {
            ret[[i]] <- get(keys[i])
        }
        ret
    }
    print <- function() {
        n <- size()
        cat("OrderedDict object with", n, "item(s)\n")
    }

    initialize(items)
    self
}



#' @title Ordered Dictionary (R implementation)
#' @description
#' The `OrderedDictL` function creates an ordered dictionary.
#' Pure R implementation for benchmarking.
#' @param items a list of items
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
#' * `key`: any R object, key of the item
#' * `value`: any R object, value of the item
#' * `default`: optinal, the default value of an item if the key is not found
#' * `d`: an OrderedDict or OrderedDictL
#' @examples
#' d <- OrderedDictL(list(apple = 5, orange = 10))
#' d$set("banana", 3)
#' d$get("apple")
#' d$as_list()  # the order the item is preserved
#' d$pop("orange")
#' d$as_list()  # "orange" is removed
#' d$set("orange", 3)$set("pear", 7)  # chain methods
#' @seealso [Dict] and [OrderedDict]
#' @importFrom utils hasName
#' @export
OrderedDictL <- function(items = NULL) {
    self <- environment()
    e <- NULL

    initialize <- function(items = NULL) {
        clear()
        for (argname in names(items)) {
            set(argname, items[[argname]])
        }
    }
    set <- function(key, value) {
        if (is.null(value)) {
            e[key] <<- list(value)
        } else  {
            e[[key]] <<- value
        }
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
        v <- keys() != key
        if (all(v)) stop("key not found")
        e <<- e[v]
        invisible(self)
    }
    pop <- function(key, default) {
        v <- get(key, default)
        remove(key)
        v
    }
    popitem <- function(last = TRUE) {
        if (last) {
            keys <- keys()
            key <- key[length(keys)]
        } else {
            keys <- keys()[1]
        }
        v <- get(key)
        remove(key)
        list(key = key, vlaue = v)
    }
    has <- function(key) {
        hasName(e, key)
    }
    keys <- function() {
        as.character(names(e))
    }
    values <- function() {
        ret <- vector("list", size())
        keys <- keys()
        for (i in seq_along(keys)) {
            ret[[i]] <- get(keys[i])
        }
        ret
    }
    update <- function(d) {
        for (key in d$keys()) {
            set(key, d$get(key))
        }
        self
    }
    clear <- function() {
        e <<- list()
        invisible(self)
    }
    size <- function() length(e)
    as_list <- function() e
    print <- function() {
        n <- size()
        cat("OrderedDictL object with", n, "item(s)\n")
    }

    initialize(items)
    self
}
