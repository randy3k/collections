#' @title Double Ended Queue
#' @description
#' The `Deque` class creates a double ended queue with pairlist backend.
#' It is recommended for long queues.
#' @section Usage:
#' \preformatted{
#' Deque$new()
#' Deque$push(item)
#' Deque$pushleft(item)
#' Deque$pop()
#' Deque$popleft()
#' Deque$extend(q)
#' Deque$extendleft(q)
#' Deque$remove(item)
#' Deque$size()
#' Deque$as_list()
#' }
#' @section Arguments:
#' * `item`: any R object
#' * `q`: a Deque object
#' @examples
#' q <- Deque$new()
#' q$push("foo")
#' q$push("bar")
#' q$pushleft("baz")
#' q$pop()  # bar
#' q$popleft()  # baz
#' @seealso [DequeL]
#' @export
Deque <- R6::R6Class("Deque",
    cloneable = FALSE,
    private = list(
        q = NULL,
        last = NULL
    ),
    public = list(
        push = function(item) {
            invisible(.Call("deque_push", PACKAGE = "collections", private, item))
        },
        pushleft = function(item) {
            invisible(.Call("deque_pushleft", PACKAGE = "collections", private, item))
        },
        pop = function() {
            .Call("deque_pop", PACKAGE = "collections", private)
        },
        popleft = function() {
            .Call("deque_popleft", PACKAGE = "collections", private)
        },
        extend = function(deque) {
            !inherits(deque, "Deque") && stop("expect Deque object")
            q <- deque$.__enclos_env__$private$q
            while (!is.null(q)) {
                v <- .Call("pairlist_car", PACKAGE = "collections", q)
                self$push(v[[2]])
                q <- .Call("pairlist_cdr", PACKAGE = "collections", q)
            }
            invisible(self)
        },
        extendleft = function(deque) {
            !inherits(deque, "Deque") && stop("expect Deque object")
            q <- deque$.__enclos_env__$private$last
            while (!is.null(q)) {
                v <- .Call("pairlist_car", PACKAGE = "collections", q)
                self$pushleft(v[[2]])
                q <- v[[1]]
            }
            invisible(self)
        },
        remove = function(item) {
            invisible(.Call("deque_remove", PACKAGE = "collections", private, item))
        },
        size = function() length(private$q),
        as_list = function() {
            ret <- list()
            i <- 0
            x <- private$q
            while (!is.null(x)) {
                i <- i + 1
                ret[[i]] <- .Call("pairlist_car", PACKAGE = "collections", x)[[2]]
                x <- .Call("pairlist_cdr", PACKAGE = "collections", x)
            }
            ret
        }
    )
)


#' @title Double Ended Queue (list based)
#' @description
#' The `DequeL` class creates a double ended queue with list backend.
#' It is recommended for short queues.
#' @section Usage:
#' \preformatted{
#' DequeL$new()
#' DequeL$push(item)
#' DequeL$pushleft(item)
#' DequeL$pop()
#' DequeL$popleft()
#' DequeL$extend(q)
#' DequeL$extendleft(q)
#' DequeL$remove(item)
#' DequeL$size()
#' DequeL$as_list()
#' }
#' @section Arguments:
#' * `item`: any R object
#' * `q`: a DequeL object
#' @examples
#' q <- DequeL$new()
#' q$push("foo")
#' q$push("bar")
#' q$pushleft("baz")
#' q$pop()  # bar
#' q$popleft()  # baz
#' @seealso [Deque]
#' @export
DequeL <- R6::R6Class("DequeL",
    cloneable = FALSE,
    private = list(
        q = list(),
        n = 0
    ),
    public = list(
        push = function(item) {
            private$q[[private$n + 1]] <- item
            private$n <- private$n + 1
            invisible(item)
        },
        pushleft = function(item) {
            private$q <- c(item, private$q)
            private$n <- private$n + 1
            invisible(item)
        },
        pop = function() {
            v <- private$q[[private$n]]
            private$q <- private$q[-private$n]
            private$n <- private$n - 1
            v
        },
        popleft = function() {
            v <- private$q[[1]]
            private$q <- private$q[-1]
            private$n <- private$n - 1
            v
        },
        extend = function(deque) {
            !inherits(deque, "DequeL") && stop("expect DequeL object")
            q <- deque$.__enclos_env__$private$q
            private$q <- c(private$q, q)
            invisible(self)
        },
        extendleft = function(deque) {
            !inherits(deque, "DequeL") && stop("expect DequeL object")
            q <- deque$.__enclos_env__$private$q
            private$q <- c(rev(q), private$q)
            invisible(self)
        },
        remove = function(value) {
            ind <- match(value, private$q)
            if (is.na(ind)) stop("value not found")
            private$q <- private$q[-ind]
            invisible(value)
        },
        size = function() length(private$q),
        as_list = function() private$q
    )
)
