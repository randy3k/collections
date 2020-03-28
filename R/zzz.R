#' @rdname deque
#' @export
Deque <- function(items = NULL) {
    deque(items)
}


#' @rdname dict
#' @export
Dict <- function(items = NULL, keys = NULL) {
    dict(items, keys)
}


#' @rdname ordered_dict
#' @export
OrderedDict <- function(items = NULL, keys = NULL) {
    ordered_dict(items, keys)
}


#' @rdname priority_queue
#' @export
PriorityQueue <- function(items = NULL, priorities = rep(0, length(items))) {
    priority_queue(items, priorities)
}


#' @rdname queue
#' @export
Queue <- function(items = NULL) {
    queue(items)
}


#' @rdname stack
#' @export
Stack <- function(items = NULL) {
    stack(items)
}
