# an R implemention of queue
# inspired by https://github.com/wch/qstack/blob/master/R/queue.R

#' @title Queue
#' @description
#' The `Queue` function creates a queue.
#' @section Usage:
#' \preformatted{
#' Queue(items = NULL)
#' .$push(item)
#' .$pop()
#' .$peek()
#' .$clear()
#' .$size()
#' .$as_list()
#' }
#' @section Argument:
#' * `items`: initialization list
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
#' @seealso [QueueL]
#' @export
Queue <- function(...) {
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

    initialize(...)
    class(self) <- "Queue"
    self
}


#' @method print Queue
#' @export
print.Queue <- function(self) {
    n <- self$size()
    cat("Queue object with", n, "item(s)\n")
}


#' @title QueueL (R implementation)
#' @description
#' The `QueueL` function creates a queue.
#' Pure R implementation for benchmarking.
#' @section Usage:
#' \preformatted{
#' QueueL(items = NULL)
#' .$push(item)
#' .$pop()
#' .$peek()
#' .$clear()
#' .$size()
#' .$as_list()
#' }
#' @section Argument:
#' * `items`: initialization list
#' * `item`: any R object
#' @examples
#' q <- QueueL()
#' q$push("first")
#' q$push("second")
#' q$pop()  # first
#' q$pop()  # second
#'
#' q <- QueueL(list("foo", "bar"))
#' q$push("baz")$push("bla")
#' @seealso [Queue]
#' @export
QueueL <- function(...) {
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
        q[[n + 1]] <<- item
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

    initialize(...)
    class(self) <- "QueueL"
    self
}

#' @method print QueueL
#' @export
print.QueueL <- function(self) {
    n <- self$size()
    cat("QueueL object with", n, "item(s)\n")
}
