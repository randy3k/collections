#' @title Double Ended Queue
#' @description
#' `deque` creates a double ended queue.
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
#' * `q`: a deque object
#' @examples
#' q <- deque()
#' q$push("foo")
#' q$push("bar")
#' q$pushleft("baz")
#' q$pop()  # bar
#' q$popleft()  # baz
#'
#' q <- deque(list("foo", "bar"))
#' q$push("baz")$pushleft("bla")
#' @seealso [queue] and [stack]
#' @export
deque <- function(items = NULL) {
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
        cat("deque object with", n, "item(s)\n")
    }

    initialize(items)
    items <- NULL
    self
}
