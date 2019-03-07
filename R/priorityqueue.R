#' @title Priority Queue
#' @description
#' The `PriorityQueue` class creates a priority queue (a.k.a heap).
#' @section Usage:
#' \preformatted{
#' PriorityQueue$new(items = NULL, priorities = NULL)
#' PriorityQueue$push(item, priority = 0)
#' PriorityQueue$pop()
#' PriorityQueue$clear()
#' PriorityQueue$size()
#' PriorityQueue$as_list()
#' }
#' @section Argument:
#' * `items`: initialization list
#' * `priorities`: a vector of priorities of the same length of `items`
#' * `item`: any R object
#' * `priority`: a real number, item with larger priority pops first
#' @examples
#' q <- PriorityQueue$new()
#' q$push("not_urgent")
#' q$push("urgent", priority = 2)
#' q$push("not_as_urgent", priority = 1)
#' q$pop()  # urgent
#' q$pop()  # not_as_urgent
#' q$pop()  # not_urgent
#'
#' q <- PriorityQueue$new(list("not_urgent", "urgent"), c(0, 2))
#' q$push("not_as_urgent", 1)$push("not_urgent2")
#' @export
#' @export
PriorityQueue <- R6::R6Class("PriorityQueue",
    cloneable = FALSE,
    private = list(
        h = NULL,
        n = NULL
    ),
    public = list(
        initialize = function(items = NULL, priorities = rep(0, length(items))) {
            self$clear()
            for (i in seq_along(items)) {
                self$push(items[[i]], priorities[i])
            }
        },
        push = function(item, priority = 0) {
            .Call("heap_push", PACKAGE = "collections", private, item, priority)
            invisible(self)
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
            ret <- vector("list", self$size())
            for (i in seq_len(private$n)) {
                ret[[i]] <- private$h[[ord[i]]][[2]]
            }
            ret
        },
        print = function() {
            n <- self$size()
            cat("PriorityQueue object with", n, "item(s).\n")
        }
    )
)
