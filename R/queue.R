# an R implemention of queue
# inspired by https://github.com/wch/qstack/blob/master/R/queue.R

#' @title Queue
#' @description
#' `queue` creates a queue. `Queue` is deprecated and will be removed from future releases.
#' @param items a list of items
#' @details
#' Following methods are exposed:
#' \preformatted{
#' .$push(item)
#' .$pop()
#' .$peek()
#' .$clear()
#' .$size()
#' .$as_list()
#' .$print()
#' }
#' * `item`: any R object
#' @examples
#' q <- queue()
#' q$push("first")
#' q$push("second")
#' q$pop()  # first
#' q$pop()  # second
#'
#' q <- queue(list("foo", "bar"))
#' q$push("baz")$push("bla")
#' @seealso [stack] and [deque]
#' @export
queue <- function(items = NULL) {
    ret <- create_queue()
    ret$initialize(items)
    ret
}


create_queue <- function() {
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
        .Call(C_queue_push, self, item)
        invisible(ret)
    }
    pop <- function() {
        .Call(C_queue_pop, self)
    }
    peek <- function() {
        if (is.null(q)) stop("queue is empty")
        .Call(C_pairlist_car, q)
    }
    clear <- function() {
        q <<- NULL
        last <<- NULL
        invisible(ret)
    }
    size <- function() length(q)
    as_list <- function() as.list(q)
    print <- function() {
        n <- size()
        cat("Queue object with", n, "item(s)\n")
    }

    ret$initialize <- initialize
    ret$push <- push
    ret$pop <- pop
    ret$peek <- peek
    ret$clear <- clear
    ret$size <- size
    ret$as_list <- as_list
    ret$print <- print
    ret
}
