#' @export
Deque <- R6::R6Class("Deque",
    cloneable = FALSE,
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
            if (is.null(private$last)) stop("deque is empty")
            v <- pairlist_car(private$last)
            private$last <- v$prev
            if (is.null(private$last)) {
                private$q <- NULL
            } else {
                pairlist_setcdr(private$last, NULL)
            }
            v$item
        },
        popleft = function() {
            if (is.null(private$q)) stop("deque is empty")
            nextq <- pairlist_cdr(private$q)
            if (is.null(nextq)) {
                private$last <- NULL
            } else {
                v <- pairlist_car(nextq)
                pairlist_setcar(nextq, list(prev = NULL, item = v$item))
            }
            v <- pairlist_car(private$q)
            private$q <- nextq
            v$item
        },
        extend = function(deque) {
            !inherits(deque, "Deque") && stop("expect Deque object")
            q <- deque$.__enclos_env__$private$q
            while (!is.null(q)) {
                v <- pairlist_car(q)
                self$push(v$item)
                q <- pairlist_cdr(q)
            }
            invisible(self)
        },
        extendleft = function(deque) {
            !inherits(deque, "Deque") && stop("expect Deque object")
            q <- deque$.__enclos_env__$private$last
            while (!is.null(q)) {
                v <- pairlist_car(q)
                self$pushleft(v$item)
                q <- v$prev
            }
            invisible(self)
        },
        remove = function(value) {
            .Call("deque_remove", PACKAGE = "collections", private, value)
            invisible(NULL)
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
