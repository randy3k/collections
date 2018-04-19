#' @export
Deque <- R6::R6Class("Deque",
    private = list(
        q = NULL,
        last = NULL
    ),
    public = list(
        push = function(item) {
            if (is.null(private$q)) {
                private$q <- private$last <- as.pairlist(list(list(prev = NULL, item = item)))
            } else {
                private$last <- pairlist_append(
                    private$last, list(prev = private$last, item = item))
            }
            invisible(item)
        },
        pushleft = function(item) {
            if (is.null(private$q)) {
                private$q <- private$last <- as.pairlist(list(list(prev = NULL, item = item)))
            } else {
                head <- pairlist_prepend(private$q, list(prev = NULL, item = item))
                v <- pairlist_car(private$q)
                pairlist_setcar(private$q, list(prev = head, item = v$item))
                private$q <- head
            }
            invisible(item)
        },
        pop = function() {
            v <- pairlist_car(private$last)
            if (!is.null(v$prev)) {
                private$last <- v$prev
                pairlist_setcdr(private$last, NULL)
            }
            v$item
        },
        popleft = function() {
            v <- pairlist_car(private$q)
            private$q <- pairlist_cdr(private$q)
            v$item
        },
        size = function() length(private$q),
        as_list = function() {
            ret <- list()
            i <- 0
            x <- private$q
            while (!is.null(x)) {
                i <- i + 1
                ret[[i]] <- pairlist_car(x)$item
                x <- pairlist_cdr(x)
            }
            ret
        }
    )
)
