#' @title Priority Queue
#' @description
#' `priority_queue` creates a priority queue (a.k.a heap).
#' @param items a list of items
#' @param priorities a vector of interger valued priorities
#' @details
#' Following methods are exposed:
#' \preformatted{
#' .$push(item, priority = 0)
#' .$pop()
#' .$clear()
#' .$size()
#' .$as_list()
#' .$print()
#' }
#' * `item`: any R object
#' * `priority`: a real number, item with larger priority pops first
#' @examples
#' q <- priority_queue()
#' q$push("not_urgent")
#' q$push("urgent", priority = 2)
#' q$push("not_as_urgent", priority = 1)
#' q$pop()  # urgent
#' q$pop()  # not_as_urgent
#' q$pop()  # not_urgent
#'
#' q <- priority_queue(list("not_urgent", "urgent"), c(0, 2))
#' q$push("not_as_urgent", 1)$push("not_urgent2")
#' @export
priority_queue <- function(items = NULL, priorities = rep(0, length(items))) {
    self <- environment()
    h <- NULL
    n <- NULL

    initialize <- function(items, priorities) {
        clear()
        for (i in seq_along(items)) {
            push(items[[i]], priorities[i])
        }
    }
    push <- function(item, priority = 0) {
        .Call(C_heap_push, self, item, priority)
        invisible(self)
    }
    pop <- function() {
        .Call(C_heap_pop, self)
    }
    clear <- function() {
        h <<- list()
        n <<- 0
        invisible(self)
    }
    size <- function() n
    as_list <- function() {
        priorities <- sapply(seq_len(n), function(i) h[[i]][[1]])
        ord <- order(priorities, decreasing = TRUE)
        ret <- vector("list", size())
        for (i in seq_len(n)) {
            ret[[i]] <- h[[ord[i]]][[2]]
        }
        ret
    }
    print <- function() {
        n <- size()
        cat("priority_queue object with", n, "item(s)\n")
    }

    initialize(items, priorities)
    items <- NULL
    priorities <- NULL
    self
}
