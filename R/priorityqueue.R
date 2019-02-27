#' @title Priority Queue
#' @description
#' The `PriorityQueue` class creates a priority queue (a.k.a heap).
#' @section Usage:
#' \preformatted{
#' PriorityQueue$new()
#' PriorityQueue$push(item, priority = 0)
#' PriorityQueue$pop()
#' PriorityQueue$clear()
#' PriorityQueue$size()
#' PriorityQueue$as_list()
#' }
#' @section Argument:
#' * `item`: any R object
#' * `priority`: non-negative interger, item with larger priority pops first
#' @examples
#' q <- PriorityQueue$new()
#' q$push("not_urgent")
#' q$push("urgent", priority = 2)
#' q$push("not_as_urgent", priority = 1)
#' q$pop()  # urgent
#' q$pop()  # not_as_urgent
#' q$pop()  # not_urgent
#' @export
#' @export
PriorityQueue <- R6::R6Class("PriorityQueue",
    cloneable = FALSE,
    private = list(
        h = NULL,
        n = NULL
    ),
    public = list(
        initialize = function() {
            self$clear()
        },
        push = function(item, priority = 0) {
            invisible(.Call("heap_push", PACKAGE = "collections", private, item, priority))
        },
        pop = function() {
            .Call("heap_pop", PACKAGE = "collections", private)
        },
        clear = function() {
            private$h <- list()
            private$n <- 0
        },
        size = function() private$n,
        as_list = function() {
            priorities <- sapply(seq_len(private$n), function(i) private$h[[i]][[1]])
            ord <- order(priorities, decreasing = TRUE)
            ret <- list()
            for (i in seq_len(private$n)) {
                ret[[i]] <- private$h[[ord[i]]][[2]]
            }
            ret
        }
    )
)
