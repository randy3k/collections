#' Deprecated Functions
#' @name deprecated
#' @param ... anything

#' @export
#' @rdname deprecated
Deque <- function(...) {
    deque(...)
}

#' @export
#' @rdname deprecated
Dict <- function(...) {
    dict(...)
}

#' @export
#' @rdname deprecated
OrderedDict <- function(...) {
    ordered_dict(...)
}

#' @export
#' @rdname deprecated
PriorityQueue <- function(...) {
    priority_queue(...)
}

#' @export
#' @rdname deprecated
Queue <- function(...) {
    queue(...)
}

#' @export
#' @rdname deprecated
Stack <- function(...) {
    stack(...)
}


#' @export
R6Classes <- list(
    Queue = R6Class("Queue", queue),
    Stack = R6Class("Stack", stack),
    Deque = R6Class("Deque", deque),
    PriorityQueue = R6Class("PriorityQueue", priority_queue),
    Dict = R6Class("Dict", dict),
    OrderedDict = R6Class("OrderedDict", ordered_dict)
)
