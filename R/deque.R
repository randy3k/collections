#' @title Double Ended Queue
#' @description
#' The `Deque` function creates a double ended queue.
#' @param items a list of items
#' @details
#' Following methods are exposed:
#' \preformatted{
#' .$push(item)
#' .$pushleft(item)
#' .$pop()
#' .$popleft()
#' .$peek()
#' .$peekleft()
#' .$extend(q)
#' .$extendleft(q)
#' .$remove(item)
#' .$clear()
#' .$size()
#' .$as_list()
#' .$print()
#' }
#' * `item`: any R object
#' * `q`: a Deque object
#' @examples
#' q <- Deque()
#' q$push("foo")
#' q$push("bar")
#' q$pushleft("baz")
#' q$pop()  # bar
#' q$popleft()  # baz
#'
#' q <- Deque(list("foo", "bar"))
#' q$push("baz")$pushleft("bla")
#' @seealso [DequeL]
#' @export
Deque <- function(items = NULL) {
    self <- environment()
    q <- NULL
    last <- NULL

    initialize <- function(items = NULL) {
        clear()
        for (i in seq_along(items)) {
            push(items[[i]])
        }
    }
    push <- function(item) {
        .Call(C_deque_push, self, item)
        invisible(self)
    }
    pushleft <- function(item) {
        .Call(C_deque_pushleft, self, item)
        invisible(self)
    }
    pop <- function() {
        .Call(C_deque_pop, self)
    }
    popleft <- function() {
        .Call(C_deque_popleft, self)
    }
    peek <- function() {
        if (is.null(last)) stop("deque is empty")
        .Call(C_pairlist_car, last)[[2]]
    }
    peekleft <- function() {
        if (is.null(q)) stop("deque is empty")
        .Call(C_pairlist_car, q)[[2]]
    }
    extend <- function(deque) {
        q <- deque$q
        while (!is.null(q)) {
            v <- .Call(C_pairlist_car, q)
            push(v[[2]])
            q <- .Call(C_pairlist_cdr, q)
        }
        invisible(self)
    }
    extendleft <- function(deque) {
        q <- deque$q
        while (!is.null(q)) {
            v <- .Call(C_pairlist_car, q)
            pushleft(v[[2]])
            q <- .Call(C_pairlist_cdr, q)
        }
        invisible(self)
    }
    clear <- function() {
        q <<- NULL
        last <<- NULL
        invisible(self)
    }
    remove <- function(item) {
        .Call(C_deque_remove, self, item)
        invisible(self)
    }
    size <- function() length(q)
    as_list <- function() {
        n <- size()
        ret <- vector("list", n)
        x <- q
        for (i in seq_len(n)) {
            ret[[i]] <- .Call(C_pairlist_car, x)[[2]]
            x <- .Call(C_pairlist_cdr, x)
        }
        ret
    }
    print <- function() {
        n <- size()
        cat("Deque object with", n, "item(s)\n")
    }

    initialize(items)
    self
}



#' @title Double Ended Queue (R implementation)
#' @description
#' The `DequeL` function creates a double ended queue.
#' Pure R implementation for benchmarking.
#' @param items a list of items
#' @details
#' Following methods are exposed:
#' \preformatted{
#' .$push(item)
#' .$pushleft(item)
#' .$pop()
#' .$popleft()
#' .$peek()
#' .$peekleft()
#' .$extend(q)
#' .$extendleft(q)
#' .$clear()
#' .$remove(item)
#' .$size()
#' .$as_list()
#' .$print()
#' }
#' * `item`: any R object
#' * `q`: a DequeL object
#' @examples
#' q <- DequeL()
#' q$push("foo")
#' q$push("bar")
#' q$pushleft("baz")
#' q$pop()  # bar
#' q$popleft()  # baz
#'
#' q <- DequeL(list("foo", "bar"))
#' q$push("baz")$pushleft("bla")
#' @seealso [Deque]
#' @export
DequeL <- function(items = NULL) {
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
    self
}
