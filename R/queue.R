# an R implemention of queue
# inspired by https://github.com/wch/qstack/blob/master/R/queue.R

#' @title Queue
#' @description
#' The `Queue` function creates a queue.
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
#' q <- Queue()
#' q$push("first")
#' q$push("second")
#' q$pop()  # first
#' q$pop()  # second
#'
#' q <- Queue(list("foo", "bar"))
#' q$push("baz")$push("bla")
#' @seealso [Stack] and [Deque]
#' @export
Queue <- function(items = NULL) {
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
        .Call(C_queue_push, self, item)
        invisible(self)
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
        invisible(self)
    }
    size <- function() length(q)
    as_list <- function() as.list(q)
    print <- function() {
        n <- size()
        cat("Queue object with", n, "item(s)\n")
    }

    initialize(items)
    items <- NULL
    self
}
