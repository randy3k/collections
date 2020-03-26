Deque <- function(items = NULL) {
    self <- environment()
    q <- NULL
    n <- NULL

    initialize <- function(items = NULL) {
        clear()
        for (i in seq_along(items)) {
            push(items[[i]])
        }
    }
    push <- function(item) {
        if (is.null(item)) {
            q[n + 1] <<- list(item)
        } else {
            q[[n + 1]] <<- item
        }
        n <<- self$n + 1
        invisible(self)
    }
    pushleft <- function(item) {
        q <<- c(list(item), q)
        n <<- n + 1
        invisible(self)
    }
    pop <- function() {
        if (n == 0) stop("deque is empty")
        v <- q[[n]]
        q <<- q[-n]
        n <<- n - 1
        v
    }
    popleft <- function() {
        if (n == 0) stop("deque is empty")
        v <- q[[1]]
        q <<- q[-1]
        n <<- n - 1
        v
    }
    peek <- function() {
        if (n == 0) stop("deque is empty")
        q[[n]]
    }
    peekleft <- function() {
        if (n == 0) stop("deque is empty")
        q[[1]]
    }
    extend <- function(deque) {
        q <<- c(q, deque$q)
        n <<- length(q)
        invisible(self)
    }
    extendleft <- function(deque) {
        q <<- c(rev(deque$q), q)
        n <<- length(q)
        invisible(self)
    }
    clear <- function() {
        q <<- list()
        n <<- 0
        invisible(self)
    }
    remove <- function(value) {
        ind <- match(value, q)
        if (is.na(ind)) stop("value not found")
        q <<- q[-ind]
        n <<- n - 1
        invisible(self)
    }
    size <- function() length(q)
    as_list <- function() q
    print <- function() {
        n <- size()
        cat("DequeL object with", n, "item(s)\n")
    }

    initialize(items)
    items <- NULL
    self
}


Dict <- function(items = NULL, keys = NULL) {
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


OrderedDict <- function(items = NULL, keys = NULL) {
    self <- environment()
    e <- NULL
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
            key <- key[[length(keys)]]
        } else {
            keys <- keys()[[1]]
        }
        v <- get(key)
        remove(key)
        list(key = key, vlaue = v)
    }
    has <- function(key) {
        hasName(e, key)
    }
    keys <- function() {
        as.list(as.character(names(e)))
    }
    values <- function() {
        ret <- vector("list", size())
        keys <- keys()
        for (i in seq_along(keys)) {
            ret[[i]] <- get(keys[[i]])
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

    initialize(items, keys0)
    items <- NULL
    keys0 <- NULL
    self
}


Queue <- function(items = NULL) {
    self <- environment()
    q <- NULL
    n <- NULL

    initialize <- function(items = NULL) {
        clear()
        for (i in seq_along(items)) {
            push(items[[i]])
        }
    }
    push <- function(item) {
        if (is.null(item)) {
            q[n + 1] <<- list(item)
        } else {
            q[[n + 1]] <<- item
        }
        n <<- n + 1
        invisible(self)
    }
    pop <- function() {
        if (n == 0) stop("queue is empty")
        v <- q[[1]]
        q <<- q[-1]
        n <<- n - 1
        v
    }
    peek <- function() {
        if (n == 0) stop("queue is empty")
        q[[1]]
    }
    clear <- function() {
        q <<- list()
        n <<- 0
        self
    }
    size <- function() n
    as_list <- function() q
    print <- function() {
        n <- size()
        cat("QueueL object with", n, "item(s)\n")
    }

    initialize(items)
    items <- NULL
    self
}


Stack <- function(items = NULL) {
    self <- environment()

    q <- NULL
    n <- NULL

    initialize <- function(items = NULL) {
        clear()
        for (i in seq_along(items)) {
            push(items[[i]])
        }
    }
    push <- function(item) {
        if (is.null(item)) {
            q[n + 1] <<- list(item)
        } else {
            q[[n + 1]] <<- item
        }
        n <<- n + 1
        invisible(self)
    }
    pop <- function() {
        if (n == 0) stop("stack is empty")
        v <- q[[n]]
        q <<- q[-n]
        n <<- n - 1
        v
    }
    peek <- function() {
        if (n == 0) stop("stack is empty")
        q[[n]]
    }
    clear <- function() {
        q <<- list()
        n <<- 0
        invisible(self)
    }
    size <- function() n
    as_list <- function() q
    print <- function() {
        n <- size()
        cat("StackL object with", n, "item(s)\n")
    }

    initialize(items)
    items <- NULL
    self
}
