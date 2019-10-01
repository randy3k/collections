#' @title Dictionary
#' @description
#' The `Dict` function creates an ordinary (unordered) dictionary (a.k.a. hash).
#' @param items a list of items
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
#' @importFrom xptr null_xptr
#' @importFrom utils hasName
#' @export
Dict <- function(items = NULL) {
    self <- environment()

    INITIAL_SIZE <- 16L
    GROW_FACTOR <- 1.5
    n <- NULL
    m <- NULL
    holes <- NULL
    nholes <- NULL
    vs <- NULL
    ks <- NULL
    ht_xptr <- NULL

    initialize <- function(items = NULL) {
        clear()
        for (argname in names(items)) {
            set(argname, items[[argname]])
        }
    }
    set <- function(key, value) {
        force(value)
        index <- .Call(C_dict_index_get, self, ht_xptr, key)
        if (index == -1L) {
            if (nholes) {
                nholes <<- nholes - 1L
                index <- holes$pop()
            } else {
                n <<- n + 1L
                index <- n
            }
            if (index > m) .grow()
            .Call(C_dict_index_set, self, ht_xptr, key, index)
            ks[index] <<- key
        }
        if (is.null(value)) {
            vs[index] <<- list(value)
        } else {
            vs[[index]] <<- value
        }
        invisible(self)
    }
    get <- function(key, default) {
        index <- .Call(C_dict_index_get, self, ht_xptr, key)
        if (index > 0L) {
            vs[[index]]
        } else if (missing(default)) {
            stop("key not found")
        } else {
            default
        }
    }
    remove <- function(key) {
        index <- .Call(C_dict_index_remove, self, ht_xptr, key)
        if (index == -1L) {
            stop("key not found")
        }
        n <<- n - 1L
        ks[index] <<- ""
        vs[index] <<- list(NULL)
        holes$push(index)
        nholes <<- nholes + 1L
        invisible(self)
    }
    pop <- function(key, default) {
        v <- get(key, default)
        remove(key)
        v
    }
    has <- function(key) {
        key %in% ks
    }
    keys <- function() {
        ks[ks != ""]
    }
    values <- function() {
        vs[ks != ""]
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
        ks <<- vector("character", INITIAL_SIZE)
        # new("externalptr") doesn't work because it returns a static pointer
        ht_xptr <<- null_xptr()
        holes <<- Queue()
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
    .grow <- function() {
        m <<- ceiling(m * GROW_FACTOR)
        ks[m] <<- NA_character_
        vs[m] <<- list(NULL)
    }

    initialize(items)
    self
}



#' @title Dictionary (R implementation)
#' @description
#' The `DictL` function creates an ordinary (unordered) dictionary (a.k.a. hash).
#' The implementation is based on R environment.
#' @param items a list of items
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
#' * `key`: any R object, key of the item
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
#' @export
DictL <- function(items = NULL) {
    self <- environment()
    e <- NULL
    n <- NULL

    initialize <- function(items = NULL) {
        clear()
        for (argname in names(items)) {
            set(argname, items[[argname]])
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

    initialize(items)
    self
}
