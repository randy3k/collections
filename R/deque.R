#' @title Double Ended Queue
#' @description
#' `deque` creates a double ended queue.
#' `Deque` is deprecated and will be removed from future releases.
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
#' * `q`: a deque
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
    ret <- create_deque()
    ret$initialize(items)
    ret
}


create_deque <- function() {
    self <- environment()
    ret <- new.env()
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
        invisible(ret)
    }
    pushleft <- function(item) {
        .Call(C_deque_pushleft, self, item)
        invisible(ret)
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
        q <- deque$self$q
        while (!is.null(q)) {
            v <- .Call(C_pairlist_car, q)
            push(v[[2]])
            q <- .Call(C_pairlist_cdr, q)
        }
        invisible(ret)
    }
    extendleft <- function(deque) {
        q <- deque$self$q
        while (!is.null(q)) {
            v <- .Call(C_pairlist_car, q)
            pushleft(v[[2]])
            q <- .Call(C_pairlist_cdr, q)
        }
        invisible(ret)
    }
    clear <- function() {
        q <<- NULL
        last <<- NULL
        invisible(ret)
    }
    remove <- function(item) {
        .Call(C_deque_remove, self, item)
        invisible(ret)
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

    ret$self <- self
    ret$initialize <- initialize
    ret$push <- push
    ret$pushleft <- pushleft
    ret$pop <- pop
    ret$popleft <- popleft
    ret$peek <- peek
    ret$peekleft <- peekleft
    ret$extend <- extend
    ret$extendleft <- extendleft
    ret$clear <- clear
    ret$remove <- remove
    ret$size <- size
    ret$as_list <- as_list
    ret$print <- print
    ret
}
